(* file: cigrid/liveness.ml *)
(* Liveness analysis for assembly instructions *)

open Asm

(* ============================================================ *)
(* Data structures for liveness analysis *)
(* ============================================================ *)

(* Virtual register - represents a variable in the program *)
type vreg = string

(* Set of virtual registers *)
module VRegSet = Set.Make(String)

(* Graph node representing one instruction with liveness information *)
type node = {
  id: int;                        (* Instruction number *)
  instr: instr;                   (* The actual instruction *)
  mutable successors: int list;   (* List of successor instruction IDs *)
  mutable def: VRegSet.t;         (* Variables defined by this instruction *)
  mutable use: VRegSet.t;         (* Variables used by this instruction *)
  mutable live_in: VRegSet.t;     (* Variables live at entry *)
  mutable live_out: VRegSet.t;    (* Variables live at exit *)
}

(* ============================================================ *)
(* Helper functions *)
(* ============================================================ *)

(* Check if a register is a hardware register (not a virtual register) *)
let is_hardware_reg reg =
  match reg with
  | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | Al | Cl -> true

(* Convert a set to a sorted list for consistent output *)
let set_to_sorted_list s =
  VRegSet.elements s |> List.sort String.compare

(* Format a set for printing *)
let format_set s =
  let elements = set_to_sorted_list s in
  "(" ^ String.concat ", " elements ^ ")"

(* ============================================================ *)
(* Virtual register extraction from operands *)
(* ============================================================ *)

(* Map from stack offset to virtual register name *)
let vreg_map = ref []
let vreg_counter = ref 0

(* Get or create a virtual register name for a stack offset *)
let get_vreg_name offset =
  try
    List.assoc offset !vreg_map
  with Not_found ->
    incr vreg_counter;
    let name = Printf.sprintf "vreg%d" !vreg_counter in
    vreg_map := (offset, name) :: !vreg_map;
    name

(* Extract virtual registers from an operand *)
let extract_vregs_from_operand op =
  match op with
  | Mem (Rbp, offset, _) when offset < 0 ->
      (* Stack-based local variable *)
      VRegSet.singleton (get_vreg_name offset)
  | Mem (reg, _, _) when not (is_hardware_reg reg) ->
      (* Memory access through a virtual register *)
      VRegSet.empty  (* Conservative: don't track complex addressing *)
  | Reg reg when not (is_hardware_reg reg) ->
      (* This shouldn't happen in our compiler, but handle it *)
      VRegSet.empty
  | _ ->
      (* Immediate, hardware register, label, or global *)
      VRegSet.empty

(* ============================================================ *)
(* Def/Use analysis for instructions *)
(* ============================================================ *)

(* Compute def and use sets for an instruction *)
let compute_def_use instr =
  match instr with
  (* Arithmetic operations: def = dst, use = src and dst *)
  | Add (dst, src) | Sub (dst, src) | Imul (dst, src)
  | And (dst, src) | Or (dst, src) | Xor (dst, src)
  | Shl (dst, src) | Sar (dst, src) ->
      let def = extract_vregs_from_operand dst in
      let use_src = extract_vregs_from_operand src in
      let use_dst = extract_vregs_from_operand dst in
      (def, VRegSet.union use_src use_dst)
  
  (* Division: uses rax and rdx, defines both *)
  | Idiv src ->
      let use = extract_vregs_from_operand src in
      (VRegSet.empty, use)  (* rax/rdx are hardware regs *)
  
  (* Sign extension: uses rax, defines rdx *)
  | Cqo ->
      (VRegSet.empty, VRegSet.empty)  (* Hardware regs only *)
  
  (* Move: def = dst, use = src *)
  | Mov (dst, src) | Movzx (dst, src) | Movsx (dst, src) ->
      let def = extract_vregs_from_operand dst in
      let use = extract_vregs_from_operand src in
      (def, use)
  
  (* Unary operations: def = op, use = op *)
  | Neg op | Not op ->
      let vreg = extract_vregs_from_operand op in
      (vreg, vreg)
  
  (* Stack operations *)
  | Push op ->
      let use = extract_vregs_from_operand op in
      (VRegSet.empty, use)
  
  | Pop op ->
      let def = extract_vregs_from_operand op in
      (def, VRegSet.empty)
  
  (* Comparison: use only *)
  | Cmp (op1, op2) ->
      let use1 = extract_vregs_from_operand op1 in
      let use2 = extract_vregs_from_operand op2 in
      (VRegSet.empty, VRegSet.union use1 use2)
  
  (* Set instructions: def = dst *)
  | Sete op | Setne op | Setl op | Setle op | Setg op | Setge op ->
      let def = extract_vregs_from_operand op in
      (def, VRegSet.empty)
  
  (* Control flow: no def/use of virtual registers *)
  | Jmp _ | Je _ | Jne _ | Call _ | Ret | Label _
  | Section _ | Directive _ | Global _ | Extern _ ->
      (VRegSet.empty, VRegSet.empty)

(* ============================================================ *)
(* Control flow graph construction *)
(* ============================================================ *)

(* Build label to instruction ID mapping *)
let build_label_map instrs =
  let map = Hashtbl.create 16 in
  List.iteri (fun i instr ->
    match instr with
    | Label lbl -> Hashtbl.add map lbl i
    | _ -> ()
  ) instrs;
  map

(* Determine successors for an instruction *)
let compute_successors instr current_id total_count label_map =
  match instr with
  (* Unconditional jump: only target *)
  | Jmp target ->
      (try [Hashtbl.find label_map target] with Not_found -> [])
  
  (* Conditional jump: target and fall-through *)
  | Je target | Jne target ->
      let target_id = try [Hashtbl.find label_map target] with Not_found -> [] in
      let fall_through = if current_id + 1 < total_count then [current_id + 1] else [] in
      target_id @ fall_through
  
  (* Return: no successors *)
  | Ret ->
      []
  
  (* Call: fall through to next instruction *)
  | Call _ ->
      if current_id + 1 < total_count then [current_id + 1] else []
  
  (* Labels, sections, directives: fall through *)
  | Label _ | Section _ | Directive _ | Global _ | Extern _ ->
      if current_id + 1 < total_count then [current_id + 1] else []
  
  (* All other instructions: fall through to next *)
  | _ ->
      if current_id + 1 < total_count then [current_id + 1] else []

(* Create graph nodes from instructions *)
let build_graph instrs =
  let label_map = build_label_map instrs in
  let total_count = List.length instrs in
  
  List.mapi (fun i instr ->
    let (def, use) = compute_def_use instr in
    let successors = compute_successors instr i total_count label_map in
    {
      id = i;
      instr = instr;
      successors = successors;
      def = def;
      use = use;
      live_in = VRegSet.empty;
      live_out = VRegSet.empty;
    }
  ) instrs

(* ============================================================ *)
(* Liveness analysis (backward dataflow) *)
(* ============================================================ *)

(* Perform iterative liveness analysis on the graph *)
let analyze_liveness nodes =
  (* Create an array for fast access by ID *)
  let node_array = Array.of_list nodes in
  let changed = ref true in
  
  (* Iterate until fixed point *)
  while !changed do
    changed := false;
    
    (* Process nodes in reverse order (backward analysis) *)
    for i = Array.length node_array - 1 downto 0 do
      let node = node_array.(i) in
      
      (* Compute new live_out: union of successors' live_in *)
      let new_live_out = List.fold_left (fun acc succ_id ->
        VRegSet.union acc node_array.(succ_id).live_in
      ) VRegSet.empty node.successors in
      
      (* Compute new live_in: use ∪ (live_out - def) *)
      let new_live_in = VRegSet.union 
        node.use 
        (VRegSet.diff new_live_out node.def) in
      
      (* Check if anything changed *)
      if not (VRegSet.equal new_live_in node.live_in) ||
         not (VRegSet.equal new_live_out node.live_out) then begin
        changed := true;
        node.live_in <- new_live_in;
        node.live_out <- new_live_out
      end
    done
  done

(* ============================================================ *)
(* Output formatting *)
(* ============================================================ *)

(* Format successor list for output *)
let format_successors succs =
  let succ_strs = List.map (fun id -> Printf.sprintf "_inst%d" id) succs in
  "[" ^ String.concat ", " succ_strs ^ "]"

(* Print a single node in the required format *)
let print_node node =
  let inst_name = Printf.sprintf "_inst%d" node.id in
  let succ_str = format_successors node.successors in
  let def_str = format_set node.def in
  let use_str = format_set node.use in
  let live_in_str = format_set node.live_in in
  let live_out_str = format_set node.live_out in
  
  Printf.printf "Node[%s, succ = %s, def = %s, use = %s, live-in = %s, live-out = %s]\n"
    inst_name succ_str def_str use_str live_in_str live_out_str

(* ============================================================ *)
(* Main entry point *)
(* ============================================================ *)

(* Main entry point: analyze instructions and print liveness graph *)
let analyze_and_print (instrs: instr list) =
  (* Reset global state *)
  vreg_map := [];
  vreg_counter := 0;
  
  (* Build graph *)
  let nodes = build_graph instrs in
  
  (* Perform liveness analysis *)
  analyze_liveness nodes;
  
  (* Print results *)
  List.iter print_node nodes
