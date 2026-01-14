(* file: ast.ml *)

(* Types - G-level adds pointers, arrays, and structs *)
type ty =
  | TInt
  | TChar
  | TVoid
  | TPtr of ty              (* Pointer types: int*, char*, etc. *)
  | TArray of ty * int      (* Array types: int[10], char[5], etc. *)
  | TStruct of string       (* Struct types: struct Foo *)
  | TIdent of string        (* Identifier types: bar, Foo (without struct keyword) *)

(* Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or | BAnd | BOr | BXor
  | Shl | Shr  (* Shift operators for G-level *)

(* Unary operators for G-level *)
type unop =
  | Neg    (* - *)
  | Not    (* ! *)
  | BNot   (* ~ *)

type expr =
  | EInt of int
  | EChar of char
  | EString of string  (* G-level: string literals *)
  | EVar of string
  | EBinOp of binop * expr * expr
  | EUnOp of unop * expr  (* Unary operators for G-level *)
  | ECall of string * (expr list)
  | EParen of expr  (* Explicitly parenthesized expression *)
  | ENew of ty * expr      (* G-level: new Type[size] *)
  | EArrayAccess of string * expr * string option   (* G-level: var[index] or var[index].field *)
  | EFieldAccess of expr * string (* G-level: obj.field *)
  | EAt of expr * int  (* annotate expression with source line *)


(* Statements - G-level adds for loop, delete, arrays *)
type stmt =
  | SExpr of expr
  | SReturn of expr option
  | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt
  | SFor of stmt option * expr option * stmt option * stmt  (* G-level: for loop *)
  | SBreak
  | SBlock of stmt list
  | SVarDef of ty * string * expr option
  | SAssign of string * expr
  | SArrayAssign of string * expr * string option * expr  (* G-level: arr[index] = value or arr[index].field = value *)
  | SFieldAssign of string * string * expr (* G-level: obj.field = value *)
  | SDelete of string                        (* G-level: delete[] varname *)


type struct_field = ty * string  (* field type and name *)

type struct_def = string * (struct_field list)  (* struct name and fields *)

type global_def =
  | GFuncDef of ty * string * ((ty * string) list) * stmt
  | GFuncDecl of ty * string * ((ty * string) list)  (* extern function declaration *)
  | GVarDef of ty * string * expr option              (* G-level: global variable definition *)
  | GVarDecl of ty * string                           (* G-level: extern global variable declaration *)
  | GStructDef of struct_def                          (* G-level: struct definition *)

type program =
  | Program of global_def list