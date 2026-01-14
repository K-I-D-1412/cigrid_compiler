(* file: pretty.ml *)

open Ast

let rec string_of_list (l: 'a list) (f: 'a -> string) (sep: string) : string =
  "{" ^ (String.concat sep (List.map f l)) ^ "}"

let rec string_of_ty = function
  | TInt -> "TInt"
  | TChar -> "TChar"
  | TVoid -> "TVoid"
  | TPtr t -> "TPoint(" ^ (string_of_ty t) ^ ")"
  | TArray (t, n) -> "TArray(" ^ (string_of_ty t) ^ ", " ^ (string_of_int n) ^ ")"
  | TStruct name -> "TStruct(\"" ^ name ^ "\")"
  | TIdent name -> "TIdent(\"" ^ name ^ "\")"

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
  | And -> "&&" | Or -> "||" | BAnd -> "&" | BOr -> "|" | BXor -> "^"
  | Shl -> "<<" | Shr -> ">>"

let string_of_unop = function
  | Neg -> "-" | Not -> "!" | BNot -> "~"

let escape_char c = match c with
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | '\000' -> "\\0"
  | '\\' -> "\\\\"
  | '\'' -> "\\'"
  | '"' -> "\\\""
  | _ -> String.make 1 c

(* Escape a string for pretty-printing *)
let escape_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    Buffer.add_string buf (escape_char c)
  ) s;
  Buffer.contents buf

(* For strings, output exactly as stored - no extra escaping needed 
   since lexer already stores escape sequences as-is *)
let string_for_output s = s

let rec string_of_expr = function
  | EInt n -> "EInt(" ^ (string_of_int n) ^ ")"
  | EChar c -> "EChar('" ^ (escape_char c) ^ "')"
  | EString s -> "EString(\"" ^ (escape_string s) ^ "\")"
  | EVar s -> "EVar(\"" ^ s ^ "\")"
    | EAt (e, _) -> string_of_expr e
  | EBinOp (op, e1, e2) ->
      "EBinOp(" ^ (string_of_binop op) ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | EUnOp (op, e) ->
      "EUnOp(" ^ (string_of_unop op) ^ ", " ^ (string_of_expr e) ^ ")"
  | ENew (t, e) ->
      "ENew(" ^ (string_of_ty t) ^ ", " ^ (string_of_expr e) ^ ")"
  | EArrayAccess (var, idx, None) ->
      "EArrayAccess(\"" ^ var ^ "\", " ^ (string_of_expr idx) ^ ", )"
  | EArrayAccess (var, idx, Some field) ->
      "EArrayAccess(\"" ^ var ^ "\", " ^ (string_of_expr idx) ^ ", \"" ^ field ^ "\")"
  | EFieldAccess (obj, field) ->
      "EFieldAccess(" ^ (string_of_expr obj) ^ ", \"" ^ field ^ "\")"
  | ECall (f, args) ->
      "ECall(\"" ^ f ^ "\", " ^ (string_of_list args string_of_expr " ") ^ ")"
  | EParen e ->
      string_of_expr e  (* EParen is transparent in AST representation *)

let rec string_of_stmt = function
  | SExpr e -> "SExpr(" ^ (string_of_expr e) ^ ")"
  | SReturn None -> "SReturn()"
  | SReturn (Some e) -> "SReturn(" ^ (string_of_expr e) ^ ")"
  | SIf (e, s1, None) ->
      "SIf(" ^ (string_of_expr e) ^ ", " ^ (string_of_stmt s1) ^ ", )"
  | SIf (e, s1, Some s2) ->
      "SIf(" ^ (string_of_expr e) ^ ", " ^ (string_of_stmt s1) ^ ", " ^ (string_of_stmt s2) ^ ")"
  | SWhile (e, s) ->
      "SWhile(" ^ (string_of_expr e) ^ ", " ^ (string_of_stmt s) ^ ")"
  | SFor (init, cond, update, body) ->
      let init_str = match init with None -> "" | Some s -> string_of_stmt s in
      let cond_str = match cond with None -> "" | Some e -> string_of_expr e in
      let update_str = match update with None -> "" | Some s -> string_of_stmt s in
      "SFor(" ^ init_str ^ ", " ^ cond_str ^ ", " ^ update_str ^ ", " ^ (string_of_stmt body) ^ ")"
  | SBreak -> "SBreak"
  | SBlock stmts ->
      "SScope(" ^ (string_of_list stmts string_of_stmt " ") ^ ")"
  | SVarDef (t, id, None) ->
      "SVarDef(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\")"
  | SVarDef (t, id, Some e) ->
      "SVarDef(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\", " ^ (string_of_expr e) ^ ")"
  | SAssign (id, e) ->
      "SVarAssign(\"" ^ id ^ "\", " ^ (string_of_expr e) ^ ")"
  | SArrayAssign (id, idx, None, e) ->
      "SArrayAssign(\"" ^ id ^ "\", " ^ (string_of_expr idx) ^ ", , " ^ (string_of_expr e) ^ ")"
  | SArrayAssign (id, idx, Some field, e) ->
      "SArrayAssign(\"" ^ id ^ "\", " ^ (string_of_expr idx) ^ ", \"" ^ field ^ "\", " ^ (string_of_expr e) ^ ")"
  | SFieldAssign (id, field, e) ->
      "SFieldAssign(\"" ^ id ^ "\", \"" ^ field ^ "\", " ^ (string_of_expr e) ^ ")"
  | SDelete id ->
      "SDelete(\"" ^ id ^ "\")"

let string_of_param (t, id) =
  "(" ^ (string_of_ty t) ^ ",\"" ^ id ^ "\")"

let string_of_global = function
  | GFuncDef (t, id, params, body) ->
      "GFuncDef(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\", " ^ 
      (string_of_list params string_of_param " ") ^ ", " ^ 
      (string_of_stmt body) ^ ")"
  | GFuncDecl (t, id, params) ->
      "GFuncDecl(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\", " ^
      (string_of_list params string_of_param " ") ^ ")"
  | GVarDecl (t, id) ->
      "GVarDecl(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\")"
  | GVarDef (t, id, None) ->
      "GVarDef(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\")"
  | GVarDef (t, id, Some e) ->
      "GVarDef(" ^ (string_of_ty t) ^ ", \"" ^ id ^ "\", " ^ (string_of_expr e) ^ ")"
  | GStructDef (name, fields) ->
      let string_of_field (t, fname) = "(" ^ (string_of_ty t) ^ ",\"" ^ fname ^ "\")" in
      "GStruct(\"" ^ name ^ "\", " ^ (string_of_list fields string_of_field " ") ^ ")"

let string_of_program = function
  | Program globals -> String.concat "\n" (List.map string_of_global globals)