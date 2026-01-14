(* file: cigrid/asm.ml *)
(* x86-64 assembly instruction representation for NASM syntax *)

(* ============================================================ *)
(* Register definitions *)
(* ============================================================ *)

(* General-purpose and special-purpose registers *)
type reg = 
  | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp  (* 64-bit registers *)
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15    (* Extended registers *)
  | Al | Cl                                         (* 8-bit registers *)

(* ============================================================ *)
(* Label formatting *)
(* ============================================================ *)

(* Prefix labels with $ to avoid NASM keyword conflicts *)
let format_label name =
  if name = "main" then "main" else "$" ^ name

(* ============================================================ *)
(* Operand types *)
(* ============================================================ *)

(* Memory access size *)
type size = 
  | Byte    (* 8-bit *)
  | Qword   (* 64-bit *)

(* Assembly operands *)
type operand = 
  | Imm of int                 (* Immediate value *)
  | Reg of reg                 (* Register *)
  | Mem of reg * int * size    (* Memory: [base + offset] with size *)
  | GlobalMem of string        (* Global variable: qword [$name] *)
  | Lbl of string              (* Label address: $label *)

(* ============================================================ *)
(* Instruction set *)
(* ============================================================ *)

(* x86-64 assembly instructions *)
type instr = 
  (* Arithmetic operations *)
  | Add of operand * operand       (* Addition *)
  | Sub of operand * operand       (* Subtraction *)
  | Imul of operand * operand      (* Signed multiplication *)
  | Idiv of operand                (* Signed division: RDX:RAX / operand *)

  (* Bitwise operations *)
  | And of operand * operand       (* Bitwise AND *)
  | Or of operand * operand        (* Bitwise OR *)
  | Xor of operand * operand       (* Bitwise XOR *)
  | Shl of operand * operand       (* Shift left *)
  | Sar of operand * operand       (* Shift arithmetic right *)

  (* Data movement *)
  | Cqo                            (* Sign-extend RAX to RDX:RAX *)
  | Mov of operand * operand       (* Move data *)
  | Movzx of operand * operand     (* Move with zero extension *)
  | Movsx of operand * operand     (* Move with sign extension *)
  | Neg of operand                 (* Negate *)
  | Not of operand                 (* Bitwise NOT *)
  
  (* Stack operations *)
  | Push of operand                (* Push to stack *)
  | Pop of operand                 (* Pop from stack *)
  | Ret                            (* Return from function *)
  
  (* Comparison and conditional *)
  | Cmp of operand * operand       (* Compare (sets flags) *)
  | Sete of operand                (* Set if equal *)
  | Setne of operand               (* Set if not equal *)
  | Setl of operand                (* Set if less *)
  | Setle of operand               (* Set if less or equal *)
  | Setg of operand                (* Set if greater *)
  | Setge of operand               (* Set if greater or equal *)
  
  (* Control flow *)
  | Jmp of string                  (* Unconditional jump *)
  | Je of string                   (* Jump if equal *)
  | Jne of string                  (* Jump if not equal *)
  | Call of string                 (* Call function *)
  | Label of string                (* Code label *)
  
  (* Assembly directives *)
  | Section of string              (* Section directive (.text, .data, .bss) *)
  | Directive of string            (* Raw directive (db, dq, etc.) *)
  | Global of string               (* Global symbol declaration *)
  | Extern of string               (* External symbol declaration *)

(* ============================================================ *)
(* NASM string conversion functions *)
(* ============================================================ *)

(* Convert register to NASM syntax *)
let string_of_reg = function
  | Rax -> "rax" | Rbx -> "rbx" | Rcx -> "rcx" | Rdx -> "rdx"
  | Rsi -> "rsi" | Rdi -> "rdi" | Rbp -> "rbp" | Rsp -> "rsp"
  | R8  -> "r8"  | R9  -> "r9"  | R10 -> "r10" | R11 -> "r11"
  | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"
  | Al  -> "al"  | Cl  -> "cl"

(* Convert operand to NASM syntax *)
let string_of_operand = function
  | Imm i -> string_of_int i
  | Reg r -> string_of_reg r
  | Mem (r, off, sz) ->  (* Memory reference with size prefix *)
      let size_str = match sz with Byte -> "byte" | Qword -> "qword" in
      if off = 0 then Printf.sprintf "%s [%s]" size_str (string_of_reg r)
      else if off > 0 then Printf.sprintf "%s [%s + %d]" size_str (string_of_reg r) off
      else Printf.sprintf "%s [%s - %d]" size_str (string_of_reg r) (-off)
  | GlobalMem s -> Printf.sprintf "qword [%s]" (format_label s)  (* Global variable access *)
  | Lbl s -> format_label s  (* Label address *)

(* Convert instruction to NASM syntax (with tab prefix for opcodes) *)
let string_of_instr = function
  | Add (o1, o2) -> Printf.sprintf "\tadd %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Sub (o1, o2) -> Printf.sprintf "\tsub %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Imul (o1, o2) -> Printf.sprintf "\timul %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Idiv o -> Printf.sprintf "\tidiv %s" (string_of_operand o)
  | And (o1, o2) -> Printf.sprintf "\tand %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Or (o1, o2) -> Printf.sprintf "\tor %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Xor (o1, o2) -> Printf.sprintf "\txor %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Shl (o1, o2) -> Printf.sprintf "\tshl %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Sar (o1, o2) -> Printf.sprintf "\tsar %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Cqo -> "\tcqo"
  | Mov (o1, o2) -> Printf.sprintf "\tmov %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Movzx (o1, o2) -> Printf.sprintf "\tmovzx %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Movsx (o1, o2) -> Printf.sprintf "\tmovsx %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Neg o -> Printf.sprintf "\tneg %s" (string_of_operand o)
  | Not o -> Printf.sprintf "\tnot %s" (string_of_operand o)
  | Ret -> "\tret"
  | Push o -> Printf.sprintf "\tpush %s" (string_of_operand o)
  | Pop o -> Printf.sprintf "\tpop %s" (string_of_operand o)
  | Cmp (o1, o2) -> Printf.sprintf "\tcmp %s, %s" (string_of_operand o1) (string_of_operand o2)
  | Sete o -> Printf.sprintf "\tsete %s" (string_of_operand o)
  | Setne o -> Printf.sprintf "\tsetne %s" (string_of_operand o)
  | Setl o -> Printf.sprintf "\tsetl %s" (string_of_operand o)
  | Setle o -> Printf.sprintf "\tsetle %s" (string_of_operand o)
  | Setg o -> Printf.sprintf "\tsetg %s" (string_of_operand o)
  | Setge o -> Printf.sprintf "\tsetge %s" (string_of_operand o)
  | Jmp s -> Printf.sprintf "\tjmp %s" (format_label s)
  | Je s -> Printf.sprintf "\tje %s" (format_label s)
  | Jne s -> Printf.sprintf "\tjne %s" (format_label s)
  | Call s -> Printf.sprintf "\tcall %s" (format_label s)
  | Label s -> Printf.sprintf "%s:" (format_label s)
  | Section s -> Printf.sprintf "section %s" s
  | Directive s -> "\t" ^ s
  | Global s -> Printf.sprintf "global %s" (format_label s)
  | Extern s -> Printf.sprintf "extern %s" (format_label s)