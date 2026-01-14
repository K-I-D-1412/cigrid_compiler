(* file: cigrid/compile.ml *)
(* AST to x86-64 assembly compiler for C-like language *)

open Ast
open Asm

(* ============================================================ *)
(* Label generation for control flow structures *)
(* ============================================================ *)

let label_counter = ref 0

let new_label prefix =
  let i = !label_counter in
  incr label_counter;
  Printf.sprintf "%s_%d" prefix i

(* ============================================================ *)
(* Global state for code generation *)
(* ============================================================ *)

(* String literals: label -> content *)
let string_constants = ref []

(* Global variables: name -> type *)
let global_vars = ref []

(* ============================================================ *)
(* Environment for local variable tracking *)
(* ============================================================ *)

(* Maps variable name to (type, stack_offset) *)
type env = {
  locals : (string * (ty * int)) list; 
  next_offset : int;  (* Next available stack offset *)
}

let empty_env = { locals = []; next_offset = 8 }

(* Variable location result *)
type var_loc = 
  | Local of ty * int    (* type, stack_offset *)
  | GlobalVar of ty      (* type *) (* Renamed from Global to GlobalVar to avoid conflict *)

(* Look up variable: check locals first, then globals *)
let lookup var env = 
  try 
    let (ty, off) = List.assoc var env.locals in
    Local (ty, off)
  with Not_found ->
    try
      let ty = List.assoc var !global_vars in
      GlobalVar ty (* CHANGED: Use new name *)
    with Not_found -> failwith ("Variable not found: " ^ var)

(* Allocate a new local variable on stack with TYPE *)
let allocate var ty env = 
  let offset = env.next_offset in
  let new_env = { 
    locals = (var, (ty, offset)) :: env.locals;
    next_offset = offset + 8 
  } in
  (offset, new_env)

(* Helper to determine element size (stride) based on type *)
let get_element_size ty =
  match ty with
  | TChar -> 1
  | TPtr TChar -> 1
  | TArray (TChar, _) -> 1
  | _ -> 8

(* ============================================================ *)
(* Expression compilation *)
(* All expressions compile to code that leaves result in RAX *)
(* ============================================================ *)

let rec compile_expr env e = 
  match e with
  (* Literals *)
  | EInt i -> [Mov (Reg Rax, Imm i)]
  | EChar c -> [Mov (Reg Rax, Imm (int_of_char c))]
  | EString s ->
      (* Generate label for string constant in .data section *)
      let lbl = new_label "str" in
      string_constants := (lbl, s) :: !string_constants;
      [Mov (Reg Rax, Lbl lbl)]  (* Load address *)
  
  (* Variable access *)
  | EVar x -> 
      (match lookup x env with
       | Local (_, offset) -> [Mov (Reg Rax, Mem (Rbp, -offset, Qword))]
       | GlobalVar _ -> [Mov (Reg Rax, GlobalMem x)])
  
  (* Parenthesized expression (transparent) *)
  | EParen e -> compile_expr env e
  
  (* Function call: AMD64 ABI (first 6 args in registers) *)
  | ECall (fname, args) ->
      let num_args = List.length args in
      let param_regs = [Reg Rdi; Reg Rsi; Reg Rdx; Reg Rcx; Reg R8; Reg R9] in
      
      let rec compile_args regs args =
        match regs, args with
        | [], [] -> []
        | r::rs, arg::rest ->
            let code = compile_expr env arg in
            code @ [Mov (r, Reg Rax)] @ (compile_args rs rest)
        | _ -> failwith "Argument count mismatch or > 6 args"
      in
      
      let used_regs = List.filteri (fun i _ -> i < num_args) param_regs in
      if num_args > 6 then failwith "More than 6 arguments not supported";
      
      (compile_args used_regs args) @ [Mov (Reg Rax, Imm 0); Call fname]

  (* Binary operators: evaluate e1 then e2, combine results *)
  | EBinOp (op, e1, e2) ->
      let code1 = compile_expr env e1 in  (* e1 -> RAX *)
      let push  = [Push (Reg Rax)] in     (* Save e1 *)
      let code2 = compile_expr env e2 in  (* e2 -> RAX *)
      let pop   = [Pop (Reg R10)] in      (* e1 -> R10, e2 in RAX *)
      
      let ops = code1 @ push @ code2 @ pop in
      
      (match op with
      (* Arithmetic operators *)
      | Add -> ops @ [Add (Reg Rax, Reg R10)]  (* RAX = e2 + e1 *)
      | Sub -> ops @ [Sub (Reg R10, Reg Rax); Mov (Reg Rax, Reg R10)]  (* RAX = e1 - e2 *)
      | Mul -> ops @ [Imul (Reg Rax, Reg R10)]  (* RAX = e2 * e1 *)
      | Div ->  (* RAX = e1 / e2 (quotient) *)
          ops @ [Mov (Reg R8, Reg Rax); Mov (Reg Rax, Reg R10); Cqo; Idiv (Reg R8)]
      | Mod ->  (* RAX = e1 % e2 (remainder) *)
          ops @ [Mov (Reg R8, Reg Rax); Mov (Reg Rax, Reg R10); Cqo; Idiv (Reg R8); Mov (Reg Rax, Reg Rdx)]
      
      (* Bitwise operators *)
      | BAnd -> ops @ [And (Reg Rax, Reg R10)]
      | BOr  -> ops @ [Or (Reg Rax, Reg R10)]
      | BXor -> ops @ [Xor (Reg Rax, Reg R10)]
      | Shl -> ops @ [Mov (Reg Rcx, Reg Rax); Mov (Reg Rax, Reg R10); Shl (Reg Rax, Reg Cl)]
      | Shr -> ops @ [Mov (Reg Rcx, Reg Rax); Mov (Reg Rax, Reg R10); Sar (Reg Rax, Reg Cl)]

      (* Logical operators: return 0 or 1 *)
      | And ->  (* Logical AND: both must be non-zero *)
          ops @ [
            Push (Reg Rax);           (* Save e2 *)
            Cmp (Reg R10, Imm 0);
            Setne (Reg Al);           (* e1 != 0 ? 1 : 0 *)
            Movzx (Reg R10, Reg Al); 
            Pop (Reg Rax);            (* Restore e2 *)
            
            Cmp (Reg Rax, Imm 0);
            Setne (Reg Al);           (* e2 != 0 ? 1 : 0 *)
            Movzx (Reg Rax, Reg Al); 
            
            And (Reg Rax, Reg R10)    (* 1 if both non-zero *)
          ]
          
      | Or ->  (* Logical OR: either must be non-zero *)
          ops @ [
             Or (Reg Rax, Reg R10);
             Cmp (Reg Rax, Imm 0);
             Setne (Reg Al);
             Movzx (Reg Rax, Reg Al)  (* 1 if either non-zero *)
          ]

      (* Comparison operators: return 0 or 1 *)
      | Eq | Neq | Lt | Le | Gt | Ge ->
          let set_instr = match op with
            | Eq -> Sete (Reg Al)   (* e1 == e2 *)
            | Neq -> Setne (Reg Al) (* e1 != e2 *)
            | Lt -> Setl (Reg Al)   (* e1 < e2 *)
            | Le -> Setle (Reg Al)  (* e1 <= e2 *)
            | Gt -> Setg (Reg Al)   (* e1 > e2 *)
            | Ge -> Setge (Reg Al)  (* e1 >= e2 *)
            | _ -> failwith "Impossible"
          in
          ops @ [Cmp (Reg R10, Reg Rax); set_instr; Movzx (Reg Rax, Reg Al)]
      )
    
  (* Unary operators *)
  | EUnOp (op, e) ->
      let code = compile_expr env e in
      (match op with
       | Neg -> code @ [Neg (Reg Rax)]  (* Arithmetic negation: -e *)
       | BNot -> code @ [Not (Reg Rax)]  (* Bitwise NOT: ~e *)
       | Not -> code @ [Cmp (Reg Rax, Imm 0); Sete (Reg Al); Movzx (Reg Rax, Reg Al)])  (* Logical NOT: !e *)

  (* Position annotation: just compile the inner expression *)
  | EAt (e, _) -> compile_expr env e

  (* Heap allocation: new type[size] *)
  | ENew (ty, size_expr) ->
      let code_size = compile_expr env size_expr in  (* size -> RAX *)
      let element_size = get_element_size ty in
      code_size @ [
        Imul (Reg Rax, Imm element_size);  (* Total bytes *)
        Mov (Reg Rdi, Reg Rax);            (* malloc(size) *)
        Call "malloc"                      (* Returns pointer in RAX *)
      ]
    
  (* Array element access: arr[index] *)
  | EArrayAccess (name, index_expr, None) -> 
      let code_idx = compile_expr env index_expr in  (* index -> RAX *)
      let (ty, load_base_instr) = match lookup name env with
        | Local (t, off) -> (t, Mov (Reg Rax, Mem (Rbp, -off, Qword)))
        | GlobalVar t -> (t, Mov (Reg Rax, GlobalMem name))
      in
      let stride = get_element_size ty in  (* Bytes per element *)
      
      code_idx @ [
        Imul (Reg Rax, Imm stride);   (* byte_offset = index * stride *)
        Mov (Reg R10, Reg Rax);       (* R10 = byte_offset *)
        load_base_instr;              (* RAX = base_address *)
        Add (Reg Rax, Reg R10);       (* RAX = base + offset *)
        Mov (Reg R10, Reg Rax);       (* R10 = element address *)
        if stride = 1 then Movzx (Reg Rax, Mem (R10, 0, Byte))  (* Load byte *)
        else Mov (Reg Rax, Mem (R10, 0, Qword))  (* Load qword *)
      ]

  | _ -> failwith "Expression not supported"

(* ============================================================ *)
(* Statement compilation *)
(* Returns (instructions, updated_environment) *)
(* ============================================================ *)

let rec compile_stmt env s =
  match s with
  (* Variable declaration with initialization *)
  | SVarDef (ty, name, Some expr) ->
      let code_expr = compile_expr env expr in
      let (offset, new_env) = allocate name ty env in 
      let code_store = [Mov (Mem (Rbp, -offset, Qword), Reg Rax)] in
      (code_expr @ code_store, new_env)

  (* Variable declaration without initialization *)
  | SVarDef (ty, name, None) ->
      let (offset, new_env) = allocate name ty env in 
      ([], new_env)

  (* Variable assignment *)
  | SAssign (name, expr) ->
      let code_expr = compile_expr env expr in
      let code_store = match lookup name env with
        | Local (_, off) -> [Mov (Mem (Rbp, -off, Qword), Reg Rax)]
        | GlobalVar _ -> [Mov (GlobalMem name, Reg Rax)]
      in
      (code_expr @ code_store, env)

  (* Return statement with optional value *)
  | SReturn (Some expr) ->
      let code_expr = compile_expr env expr in
      let epilogue = [Mov (Reg Rsp, Reg Rbp); Pop (Reg Rbp); Ret] in
      (code_expr @ epilogue, env)

  | SReturn None ->
      ([Mov (Reg Rsp, Reg Rbp); Pop (Reg Rbp); Ret], env)
    
  (* If-else statement *)
  | SIf (cond, then_stmt, else_opt) ->
      let label_else = new_label "else" in
      let label_end = new_label "end_if" in
      let code_cond = compile_expr env cond in
      let check = [Cmp (Reg Rax, Imm 0); Je label_else] in  (* Jump if false *)
      let (code_then, _) = compile_stmt env then_stmt in
      let code_else_part = match else_opt with
        | None -> [Label label_else]  (* No else branch *)
        | Some else_stmt ->
            let (code_else, _) = compile_stmt env else_stmt in
            [Jmp label_end; Label label_else] @ code_else @ [Label label_end]
      in
      (code_cond @ check @ code_then @ code_else_part, env)

  (* While loop *)
  | SWhile (cond, body) ->
      let label_start = new_label "while_start" in
      let label_end = new_label "while_end" in
      let code_cond = compile_expr env cond in
      let (code_body, _) = compile_stmt env body in
      let loop = [Label label_start] @ code_cond @ [Cmp (Reg Rax, Imm 0); Je label_end] @ code_body @ [Jmp label_start; Label label_end] in
      (loop, env)

  | SFor (init_opt, cond_opt, step_opt, body) ->
      let (code_init, env_init) = match init_opt with Some s -> compile_stmt env s | None -> ([], env) in
      let cond_expr = match cond_opt with Some e -> e | None -> EInt 1 in
      let step_stmt = match step_opt with Some s -> s | None -> SBlock [] in
      let new_body = SBlock [body; step_stmt] in
      let (code_loop, final_env) = compile_stmt env_init (SWhile (cond_expr, new_body)) in
      (code_init @ code_loop, final_env)

  | SBlock stmts -> compile_stmts env stmts
  | SExpr e -> let code = compile_expr env e in (code, env)

  (* Delete/free heap memory: delete[] ptr *)
  | SDelete name ->
      let load_ptr_instr = match lookup name env with
        | Local (_, off) -> Mov (Reg Rax, Mem (Rbp, -off, Qword))
        | GlobalVar _ -> Mov (Reg Rax, GlobalMem name)
      in
      [load_ptr_instr; Mov (Reg Rdi, Reg Rax); Call "free"], env

  (* Array element assignment: arr[index] = value *)
  | SArrayAssign (name, index_expr, None, val_expr) ->
      let code_val = compile_expr env val_expr in  (* value -> RAX *)
      let push_val = [Push (Reg Rax)] in           (* Save value *)
      let code_idx = compile_expr env index_expr in (* index -> RAX *)
      
      let (ty, load_base_instr) = match lookup name env with
        | Local (t, off) -> (t, Mov (Reg Rax, Mem (Rbp, -off, Qword)))
        | GlobalVar t -> (t, Mov (Reg Rax, GlobalMem name))
      in
      let stride = get_element_size ty in

      code_val @ push_val @ code_idx @ [
        Imul (Reg Rax, Imm stride);   (* byte_offset = index * stride *)
        Mov (Reg R10, Reg Rax);       (* R10 = offset *)
        load_base_instr;              (* RAX = base address *)
        Add (Reg R10, Reg Rax);       (* R10 = destination address *)
        Pop (Reg Rax);                (* RAX = value *)
        
        if stride = 1 then Directive "mov byte [r10], al"  (* Store byte *)
        else Mov (Mem (R10, 0, Qword), Reg Rax)  (* Store qword *)
      ], env

  | _ -> failwith "Statement not supported"

(* Compile list of statements sequentially *)
and compile_stmts env stmts =
  match stmts with
  | [] -> ([], env)
  | s :: rest ->
      let (code1, env1) = compile_stmt env s in
      let (code2, env2) = compile_stmts env1 rest in
      (code1 @ code2, env2)

(* ============================================================ *)
(* Function compilation *)
(* ============================================================ *)

(* Process function parameters: move from registers to stack *)
let compile_params params env =
  let param_regs = [Reg Rdi; Reg Rsi; Reg Rdx; Reg Rcx; Reg R8; Reg R9] in
  let rec process_params params regs env instrs =
    match params, regs with
    | [], _ -> (env, instrs)
    | (ty, name) :: rest_params, reg :: rest_regs ->
        let (offset, new_env) = allocate name ty env in 
        let move_instr = Mov (Mem (Rbp, -offset, Qword), reg) in
        process_params rest_params rest_regs new_env (instrs @ [move_instr])
    | _ -> failwith "Too many arguments (max 6 supported)"
  in
  process_params params param_regs env []

(* Compile function definition *)
let compile_func g_def = 
  match g_def with
  | GFuncDef (_, name, params, body) ->
      (* Function prologue *)
      let prologue = [Label name; Push (Reg Rbp); Mov (Reg Rbp, Reg Rsp)] in
      
      (* Move parameters from registers to stack *)
      let (env_with_params, param_instrs) = compile_params params empty_env in
      
      (* Compile function body *)
      let stmts = match body with SBlock ss -> ss | _ -> [body] in
      let (body_code, final_env) = compile_stmts env_with_params stmts in
      
      (* Calculate stack size (16-byte aligned for AMD64 ABI) *)
      let stack_used = final_env.next_offset in
      let stack_size = if stack_used mod 16 = 0 then stack_used else stack_used + (16 - (stack_used mod 16)) in
      let stack_alloc = [Sub (Reg Rsp, Imm stack_size)] in
      
      (* Default epilogue (in case function doesn't return explicitly) *)
      let default_epilogue = [Mov (Reg Rsp, Reg Rbp); Pop (Reg Rbp); Ret] in
      
      prologue @ stack_alloc @ param_instrs @ body_code @ default_epilogue
  | _ -> failwith "compile_func: Expected GFuncDef"

(* ============================================================ *)
(* Program compilation *)
(* ============================================================ *)

(* Collect extern function declarations *)
let collect_externs (Program globals) =
  List.filter_map (function GFuncDecl(_, name, _) -> Some name | _ -> None) globals

(* Compile string constant to NASM data section *)
let compile_string_data (lbl, str) =
  let char_list = List.init (String.length str) (String.get str) in
  let ascii_list = List.map (fun c -> string_of_int (int_of_char c)) char_list in
  let bytes_str = String.concat ", " (ascii_list @ ["0"]) in  (* Null-terminated *)
  [Label lbl; Directive ("db " ^ bytes_str)]

(* Main program compilation entry point *)
let compile_program (Program globals) =
  (* Reset global state *)
  string_constants := [];
  global_vars := [];
  
  (* Pass 1: Collect global variables for symbol resolution *)
  List.iter (function
    | GVarDef (ty, name, _) -> global_vars := (name, ty) :: !global_vars
    | _ -> ()
  ) globals;

  (* Generate extern declarations *)
  let extern_names = collect_externs (Program globals) in
  let all_externs = List.sort_uniq String.compare (extern_names @ ["malloc"; "free"; "printf"; "puts"; "putchar"]) in 
  let extern_decls = List.map (fun name -> Extern name) all_externs in
  
  (* Assembly file header *)
  let header = extern_decls @ [Global "main"; Section ".text"] in
  
  (* Separate functions and variables *)
  let funcs = List.filter (function GFuncDef _ -> true | _ -> false) globals in
  let vars  = List.filter (function GVarDef _ -> true | _ -> false) globals in

  (* Compile all functions (.text section) *)
  let func_codes = List.concat (List.map compile_func funcs) in

  (* Compile global variables (.data / .bss sections) *)
  let var_codes = List.concat (List.map (fun g ->
    match g with
    | GVarDef (_, name, Some (EInt v)) ->  (* Initialized *)
        [Global name; Section ".data"; Label name; Directive ("dq " ^ string_of_int v)] 
    | GVarDef (_, name, None) ->  (* Uninitialized *)
        [Global name; Section ".bss"; Label name; Directive "resq 1"]
    | _ -> []
  ) vars) in

  (* Compile string literals (.data section) *)
  let string_data_code = 
    if !string_constants = [] then []
    else [Section ".data"] @ List.concat (List.map compile_string_data !string_constants)
  in
  
  (* Combine all sections *)
  header @ func_codes @ var_codes @ string_data_code