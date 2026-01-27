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

(* Location information *)
type location = {
  line: int;
  column: int;
}

(* Note: If you need a placeholder location, use { line = 0; column = 0 } directly *)

(* Statements - G-level adds for loop, delete, arrays *)
(* Each statement now carries its source location *)
type stmt =
  | SExpr of expr * location
  | SReturn of expr option * location
  | SIf of expr * stmt * stmt option * location
  | SWhile of expr * stmt * location
  | SFor of stmt option * expr option * stmt option * stmt * location  (* G-level: for loop *)
  | SBreak of location
  | SBlock of stmt list * location
  | SVarDef of ty * string * expr option * location
  | SAssign of string * expr * location
  | SArrayAssign of string * expr * string option * expr * location  (* G-level: arr[index] = value or arr[index].field = value *)
  | SFieldAssign of string * string * expr * location (* G-level: obj.field = value *)
  | SDelete of string * location                        (* G-level: delete[] varname *)

(* Helper function to extract location from a statement *)
let stmt_location (s: stmt) : location =
  match s with
  | SExpr (_, loc) -> loc
  | SReturn (_, loc) -> loc
  | SIf (_, _, _, loc) -> loc
  | SWhile (_, _, loc) -> loc
  | SFor (_, _, _, _, loc) -> loc
  | SBreak loc -> loc
  | SBlock (_, loc) -> loc
  | SVarDef (_, _, _, loc) -> loc
  | SAssign (_, _, loc) -> loc
  | SArrayAssign (_, _, _, _, loc) -> loc
  | SFieldAssign (_, _, _, loc) -> loc
  | SDelete (_, loc) -> loc

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