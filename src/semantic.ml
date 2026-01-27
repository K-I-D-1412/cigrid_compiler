(* Semantic analysis for Cigrid compiler *)
(* Implements name analysis and type checking *)

open Ast

(* ============================================================ *)
(* Exception types *)
(* ============================================================ *)

exception SemanticError of string * int  (* error message, line number *)
exception NameError of string * int      (* undefined name, line number *)
exception TypeError of string * int      (* type error, line number *)

(* ============================================================ *)
(* Environment for storing variable and function definitions *)
(* ============================================================ *)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type var_env = ty StringMap.t  (* Maps variable names to their types *)
type func_env = (ty * ty list) StringMap.t  (* Maps function names to (return_type, param_types) *)
type struct_env = (struct_field list) StringMap.t

(* ============================================================ *)
(* Type validation and struct field lookup *)
(* ============================================================ *)

let rec validate_type (struct_env: struct_env) (t: ty) : unit =
  match t with
  | TInt | TChar | TVoid -> ()
  | TStruct s | TIdent s ->
      if not (StringMap.mem s struct_env) then
        raise (TypeError ("Unknown struct: " ^ s, 0))
  | TPtr t' -> validate_type struct_env t'
  | TArray (t', _) -> validate_type struct_env t'

(* Lookup a struct field's type, raising a TypeError if missing *)
let lookup_struct_field (struct_env: struct_env) (struct_name: string) (field: string) (line: int) : ty =
  match StringMap.find_opt struct_name struct_env with
  | None -> raise (TypeError ("Unknown struct: " ^ struct_name, line))
  | Some fields ->
      let rec find_field lst =
        match lst with
        | [] -> raise (TypeError ("Unknown field '" ^ field ^ "' in struct " ^ struct_name, line))
        | (fty, fname) :: tl -> if fname = field then fty else find_field tl
      in
      find_field fields

(* ============================================================ *)
(* Name Analysis (Task 3.1) *)
(* ============================================================ *)

(* Check if a variable is defined in the current environment *)
let check_var_defined (env: var_env) (name: string) (line: int) : unit =
  if not (StringMap.mem name env) then
    raise (NameError ("Undefined variable: " ^ name, line))

(* Extract line number from an expression *)
let rec get_expr_line (e: expr) : int =
  match e with
  | EAt (_, line) -> line
  | EBinOp (_, e1, _) -> get_expr_line e1
  | EUnOp (_, e1) -> get_expr_line e1
  | _ -> 0

let rec check_expr_names (env: var_env) (e: expr) : unit =
  match e with
  | EInt _ | EChar _ | EString _ -> ()
  | EAt (e, _) -> check_expr_names env e
  | EVar name -> 
      check_var_defined env name (get_expr_line e)
  | EBinOp (_, e1, e2) ->
      check_expr_names env e1;
      check_expr_names env e2
  | EUnOp (_, e) ->
      check_expr_names env e
  | EParen e ->
      check_expr_names env e
  | ECall (fname, args) ->
      List.iter (check_expr_names env) args
  | ENew (_, e) ->
      check_expr_names env e
  | EArrayAccess (name, index, _) ->
      check_var_defined env name (get_expr_line e);
      check_expr_names env index
  | EFieldAccess (e, _) ->
      check_expr_names env e

let rec check_stmt_names (env: var_env) (s: stmt) : var_env =
  match s with
  | SExpr (e, loc) ->
      check_expr_names env e;
      env
  | SVarDef (ty, name, Some e, loc) ->
      check_expr_names env e;
      StringMap.add name ty env
  | SVarDef (ty, name, None, loc) ->
      StringMap.add name ty env
  | SAssign (name, e, loc) ->
      check_var_defined env name loc.line;
      check_expr_names env e;
      env
  | SArrayAssign (name, index, _, e, loc) ->
      check_var_defined env name loc.line;
      check_expr_names env index;
      check_expr_names env e;
      env
  | SFieldAssign (name, _, e, loc) ->
      check_var_defined env name loc.line;
      check_expr_names env e;
      env
  | SDelete (name, loc) ->
      check_var_defined env name loc.line;
      env
  | SReturn (Some e, loc) ->
      check_expr_names env e;
      env
  | SReturn (None, loc) ->
      env
  | SIf (cond, then_stmt, None, loc) ->
      check_expr_names env cond;
      let _ = check_stmt_names env then_stmt in
      env
  | SIf (cond, then_stmt, Some else_stmt, loc) ->
      check_expr_names env cond;
      let _ = check_stmt_names env then_stmt in
      let _ = check_stmt_names env else_stmt in
      env
  | SWhile (cond, body, loc) ->
      check_expr_names env cond;
      let _ = check_stmt_names env body in
      env
  | SFor (init, cond, inc, body, loc) ->
      let env1 = match init with Some s -> check_stmt_names env s | None -> env in
      (match cond with Some e -> check_expr_names env1 e | None -> ());
      (match inc with Some s -> let _ = check_stmt_names env1 s in () | None -> ());
      let _ = check_stmt_names env1 body in
      env
  | SBreak loc ->
      env
  | SBlock (stmts, loc) ->
      let _ = List.fold_left check_stmt_names env stmts in
      env

let check_func_names (func: global_def) : unit =
  match func with
  | GFuncDef (_, _, params, body) ->
      let env = List.fold_left 
        (fun acc (ty, name) -> StringMap.add name ty acc) 
        StringMap.empty 
        params 
      in
      let _ = check_stmt_names env body in
      ()
  | _ -> ()

let analyze_names (prog: global_def list) : unit =
  List.iter check_func_names prog

(* ============================================================ *)
(* Type Checking (Task 3.2) *)
(* ============================================================ *)

let rec types_equal (t1: ty) (t2: ty) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TChar, TChar -> true
  | TVoid, TVoid -> true
  | TPtr t1', TPtr t2' -> types_equal t1' t2'
  | TArray (t1', n1), TArray (t2', n2) -> types_equal t1' t2' && n1 = n2
  | TStruct s1, TStruct s2 -> s1 = s2
  | TIdent s1, TIdent s2 -> s1 = s2
  | TStruct s1, TIdent s2 -> s1 = s2
  | TIdent s1, TStruct s2 -> s1 = s2
  | _ -> false

let rec type_of_expr ?(line=0) (var_env: var_env) (func_env: func_env) (struct_env: struct_env) (e: expr) : ty =
  match e with
  | EAt (inner, l) -> type_of_expr ~line:l var_env func_env struct_env inner
  | EInt _ -> TInt
  | EChar _ -> TChar
  | EString _ -> TPtr TChar
  | EVar name ->
    (try StringMap.find name var_env
     with Not_found -> raise (NameError ("Undefined variable: " ^ name, line)))
  | EBinOp (op, e1, e2) ->
    let t1 = type_of_expr ~line var_env func_env struct_env e1 in
    let t2 = type_of_expr ~line var_env func_env struct_env e2 in
    (match op with
     | Add | Sub | Mul | Div | Mod | BAnd | BOr | BXor | Shl | Shr ->
       if types_equal t1 TInt && types_equal t2 TInt then TInt
       else raise (TypeError ("Arithmetic operation requires int operands", line))
     | And | Or ->
       if types_equal t1 TInt && types_equal t2 TInt then TInt
       else raise (TypeError ("Logical operation requires int operands", line))
     | Lt | Le | Gt | Ge ->
       if types_equal t1 TInt && types_equal t2 TInt then TInt
       else raise (TypeError ("Comparison requires int operands", line))
     | Eq | Neq ->
       if types_equal t1 t2 then TInt
       else raise (TypeError ("Equality requires operands of the same type", line))
    )
  | EUnOp (op, e) ->
    let t = type_of_expr ~line var_env func_env struct_env e in
    (match op with
     | Neg | BNot ->
       if types_equal t TInt then TInt
       else raise (TypeError ("Unary operation requires int operand", line))
     | Not ->
       if types_equal t TInt then TInt
       else raise (TypeError ("Logical not requires int operand", line))
    )
  | EParen e ->
    type_of_expr ~line var_env func_env struct_env e
  | ECall (fname, args) ->
    (try
    let (ret_ty, param_tys) = StringMap.find fname func_env in
    let arg_tys = List.map (type_of_expr ~line var_env func_env struct_env) args in
    if List.length arg_tys <> List.length param_tys then
      raise (TypeError ("Function " ^ fname ^ " called with wrong number of arguments", line));
    List.iter2 (fun pt at ->
      if not (types_equal pt at) then
      raise (TypeError ("Function argument type mismatch", line))
    ) param_tys arg_tys;
    ret_ty
    with Not_found -> raise (NameError ("Undefined function: " ^ fname, line)))
  | ENew (ty, _) ->
    TPtr ty
  | EArrayAccess (name, index, field_opt) ->
    let _ =
    let idx_ty = type_of_expr ~line var_env func_env struct_env index in
    if not (types_equal idx_ty TInt) then
      raise (TypeError ("Array index must be int", line))
    in
    (try
    let var_ty = StringMap.find name var_env in
    let elem_ty =
      match var_ty with
      | TArray (elem_ty, _) -> elem_ty
      | TPtr elem_ty -> elem_ty
      | _ -> raise (TypeError ("Array access on non-array type", line))
    in
    (match field_opt with
     | None -> elem_ty
     | Some field ->
       (match elem_ty with
        | TStruct s | TIdent s -> lookup_struct_field struct_env s field line
        | _ -> raise (TypeError ("Field access on non-struct array element", line))))
    with Not_found -> raise (NameError ("Undefined variable: " ^ name, line)))
  | EFieldAccess (e, field) ->
    let t = type_of_expr ~line var_env func_env struct_env e in
    (match t with
     | TStruct s | TIdent s -> lookup_struct_field struct_env s field line
     | _ -> raise (TypeError ("Field access on non-struct value", line)))

let rec type_check_stmt (var_env: var_env) (func_env: func_env) (struct_env: struct_env) (expected_ret_ty: ty) (s: stmt) : var_env =
  match s with
  | SExpr (e, loc) ->
      let _ = type_of_expr var_env func_env struct_env e in
      var_env
  | SVarDef (ty, name, Some e, loc) ->
      validate_type struct_env ty;
      let expr_ty = type_of_expr var_env func_env struct_env e in
      if not (types_equal ty expr_ty) then
        raise (TypeError ("Type mismatch in variable declaration", loc.line));
      StringMap.add name ty var_env
  | SVarDef (ty, name, None, loc) ->
      validate_type struct_env ty;
      StringMap.add name ty var_env
  | SAssign (name, e, loc) ->
      let var_ty = (try StringMap.find name var_env 
                    with Not_found -> raise (NameError ("Undefined variable: " ^ name, loc.line))) in
      let expr_ty = type_of_expr var_env func_env struct_env e in
      if not (types_equal var_ty expr_ty) then
        raise (TypeError ("Type mismatch in assignment", loc.line));
      var_env
  | SArrayAssign (name, index, field_opt, e, loc) ->
      let idx_ty = type_of_expr var_env func_env struct_env index in
      if not (types_equal idx_ty TInt) then
        raise (TypeError ("Array index must be int", loc.line));
      let var_ty = (try StringMap.find name var_env
                    with Not_found -> raise (NameError ("Undefined variable: " ^ name, loc.line))) in
      let elem_ty =
        match var_ty with
        | TArray (elem_ty, _) -> elem_ty
        | TPtr elem_ty -> elem_ty
        | _ -> raise (TypeError ("Array assignment on non-array type", loc.line))
      in
      (match field_opt with
       | None ->
           let expr_ty = type_of_expr var_env func_env struct_env e in
           if not (types_equal elem_ty expr_ty) then
             raise (TypeError ("Type mismatch in array assignment", loc.line));
           var_env
       | Some field ->
           (match elem_ty with
            | TStruct s | TIdent s ->
                let field_ty = lookup_struct_field struct_env s field loc.line in
                let expr_ty = type_of_expr var_env func_env struct_env e in
                if not (types_equal field_ty expr_ty) then
                  raise (TypeError ("Type mismatch in struct field assignment", loc.line));
                var_env
            | _ -> raise (TypeError ("Field assignment on non-struct array element", loc.line))))
  | SFieldAssign (name, field, e, loc) ->
      let var_ty = (try StringMap.find name var_env
                    with Not_found -> raise (NameError ("Undefined variable: " ^ name, loc.line))) in
      (match var_ty with
       | TStruct s | TIdent s ->
           let field_ty = lookup_struct_field struct_env s field loc.line in
           let expr_ty = type_of_expr var_env func_env struct_env e in
           if not (types_equal field_ty expr_ty) then
             raise (TypeError ("Type mismatch in struct field assignment", loc.line));
           var_env
       | _ -> raise (TypeError ("Field assignment on non-struct variable", loc.line)))
  | SDelete (name, loc) ->
      let _ = (try StringMap.find name var_env 
               with Not_found -> raise (NameError ("Undefined variable: " ^ name, loc.line))) in
      var_env
  | SReturn (Some e, loc) ->
      let actual_ty = type_of_expr var_env func_env struct_env e in
      if not (types_equal actual_ty expected_ret_ty) then
        raise (TypeError ("Return type mismatch: expected " ^ 
                         (match expected_ret_ty with 
                          | TInt -> "int" 
                          | TChar -> "char" 
                          | TVoid -> "void"
                          | TPtr _ -> "pointer"
                          | TStruct s | TIdent s -> s
                          | _ -> "unknown") ^ 
                         " but got " ^
                         (match actual_ty with 
                          | TInt -> "int" 
                          | TChar -> "char" 
                          | TVoid -> "void"
                          | TPtr _ -> "pointer"
                          | TStruct s | TIdent s -> s
                          | _ -> "unknown"), loc.line));
      var_env
  | SReturn (None, loc) ->
      if not (types_equal expected_ret_ty TVoid) then
        raise (TypeError ("Non-void function must return a value", loc.line));
      var_env
  | SIf (cond, then_stmt, else_opt, loc) ->
      let _ = type_of_expr var_env func_env struct_env cond in
      let _ = type_check_stmt var_env func_env struct_env expected_ret_ty then_stmt in
      (match else_opt with
       | Some else_stmt -> let _ = type_check_stmt var_env func_env struct_env expected_ret_ty else_stmt in var_env
       | None -> var_env)
  | SWhile (cond, body, loc) ->
      let _ = type_of_expr var_env func_env struct_env cond in
      let _ = type_check_stmt var_env func_env struct_env expected_ret_ty body in
      var_env
  | SFor (init, cond, inc, body, loc) ->
      let env1 = match init with Some s -> type_check_stmt var_env func_env struct_env expected_ret_ty s | None -> var_env in
      (match cond with Some e -> let _ = type_of_expr env1 func_env struct_env e in () | None -> ());
      (match inc with Some s -> let _ = type_check_stmt env1 func_env struct_env expected_ret_ty s in () | None -> ());
      let _ = type_check_stmt env1 func_env struct_env expected_ret_ty body in
      var_env
  | SBreak loc ->
      var_env
  | SBlock (stmts, loc) ->
      let _ = List.fold_left (fun env stmt -> type_check_stmt env func_env struct_env expected_ret_ty stmt) var_env stmts in
      var_env

(* Build function environment from program *)
let build_func_env (prog: global_def list) : func_env =
  (* First pass: collect all function definitions (not declarations) *)
  let _ = List.fold_left (fun acc gdef ->
    match gdef with
    | GFuncDef (_, name, _, _) ->
        if StringSet.mem name acc then
          raise (TypeError ("Duplicate function definition: " ^ name, 0))
        else
          StringSet.add name acc
    | _ -> acc
  ) StringSet.empty prog in
  (* Second pass: build environment from all functions and check signature consistency *)
  List.fold_left (fun env gdef ->
    match gdef with
    | GFuncDef (ret_ty, name, params, _) | GFuncDecl (ret_ty, name, params) ->
        let param_tys = List.map fst params in
        (* Check if function already exists with different signature *)
        (match StringMap.find_opt name env with
         | Some (existing_ret, existing_params) ->
             (* Check return type match *)
             if existing_ret <> ret_ty then
               raise (TypeError ("Function '" ^ name ^ "' declared with conflicting return types", 0))
             (* Check parameter types match *)
             else if existing_params <> param_tys then
               raise (TypeError ("Function '" ^ name ^ "' declared with conflicting parameter types", 0))
             else
               env  (* Signature matches, keep existing *)
         | None ->
             StringMap.add name (ret_ty, param_tys) env)
    | _ -> env
  ) StringMap.empty prog

(* Build global variable environment from program *)
let build_global_var_env (struct_env: struct_env) (prog: global_def list) : var_env =
  List.fold_left (fun env gdef ->
    match gdef with
    | GVarDef (ty, name, _) | GVarDecl (ty, name) ->
        validate_type struct_env ty;
        if StringMap.mem name env then
          raise (TypeError ("Duplicate global variable definition: " ^ name, 0))
        else
          StringMap.add name ty env
    | _ -> env
  ) StringMap.empty prog

(* Build struct environment from program *)
let build_struct_env (prog: global_def list) : struct_env =
  List.fold_left (fun env gdef ->
    match gdef with
    | GStructDef (name, fields) ->
        if StringMap.mem name env then
          raise (TypeError ("Duplicate struct definition: " ^ name, 0))
        else
          StringMap.add name fields env
    | _ -> env
  ) StringMap.empty prog

(* Type check a function *)
let type_check_func (global_var_env: var_env) (func_env: func_env) (struct_env: struct_env) (gdef: global_def) : unit =
  match gdef with
  | GFuncDef (ret_ty, fname, params, body) ->
      validate_type struct_env ret_ty;
      List.iter (fun (pty, _) -> validate_type struct_env pty) params;
      
      (* Get the location from the function body *)
      let func_line = (stmt_location body).line in
      
      (* Check for duplicate parameter names *)
      let _ = List.fold_left
        (fun seen (_, name) ->
          if StringSet.mem name seen then
            raise (TypeError ("Duplicate parameter name: " ^ name, func_line))
          else
            StringSet.add name seen
        )
        StringSet.empty
        params
      in
      (* Start with global variables, then add parameters *)
      let var_env = List.fold_left 
        (fun acc (ty, name) -> StringMap.add name ty acc) 
        global_var_env
        params 
      in
      (* Pass the function's return type to type_check_stmt for return statement validation *)
      let _ = type_check_stmt var_env func_env struct_env ret_ty body in
      ()
  | _ -> ()

(* Main entry point for type checking *)
let type_check (prog: global_def list) : unit =
  let func_env = build_func_env prog in
  let rec process (struct_env, var_env) gdef =
    match gdef with
    | GStructDef (name, fields) ->
        (* Allow self-reference during field validation *)
        let env_with_self = StringMap.add name fields struct_env in
        List.iter (fun (fty, _) -> validate_type env_with_self fty) fields;
        if StringMap.mem name struct_env then
          raise (TypeError ("Duplicate struct definition: " ^ name, 0));
        if StringMap.mem name func_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a function and a struct", 0));
        if StringMap.mem name var_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a struct", 0));
        (env_with_self, var_env)
    | GVarDef (ty, name, init_opt) ->
        validate_type struct_env ty;
        if StringMap.mem name var_env then
          raise (TypeError ("Duplicate global variable definition: " ^ name, 0));
        if StringMap.mem name func_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a function", 0));
        if StringMap.mem name struct_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a struct", 0));
        (match init_opt with
         | Some e -> let _ = type_of_expr var_env func_env struct_env e in ()
         | None -> ());
        let var_env' = StringMap.add name ty var_env in
        (struct_env, var_env')
    | GVarDecl (ty, name) ->
        validate_type struct_env ty;
        if StringMap.mem name var_env then
          raise (TypeError ("Duplicate global variable definition: " ^ name, 0));
        if StringMap.mem name func_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a function", 0));
        if StringMap.mem name struct_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a struct", 0));
        let var_env' = StringMap.add name ty var_env in
        (struct_env, var_env')
    | GFuncDecl (ret_ty, name, params) ->
        validate_type struct_env ret_ty;
        List.iter (fun (pty, _) -> validate_type struct_env pty) params;
        if StringMap.mem name struct_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a function and a struct", 0));
        if StringMap.mem name var_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a function", 0));
        (struct_env, var_env)
    | GFuncDef (_, name, _, _) as fdef ->
        if StringMap.mem name struct_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a function and a struct", 0));
        if StringMap.mem name var_env then
          raise (TypeError ("Name conflict: '" ^ name ^ "' is both a global variable and a function", 0));
        type_check_func var_env func_env struct_env fdef;
        (struct_env, var_env)
  in
  let _ = List.fold_left process (StringMap.empty, StringMap.empty) prog in
  ()