(* Cigrid compiler main entry point *)

open Ast
open Pretty

let parse_args () =
  let input_file = ref "" in
  let pretty_print = ref false in
  let line_error = ref false in
  let generate_asm = ref false in
  let liveness = ref false in
  let name_analysis = ref false in
  let type_check = ref false in

  let arg_spec = [
    ("--pretty-print", Arg.Set pretty_print, "Pretty-print the AST");
    ("--line-error", Arg.Set line_error, "Print only line number for errors");
    ("--asm", Arg.Set generate_asm, "Generate x86-64 assembly");
    ("--liveness", Arg.Set liveness, "Perform liveness analysis");
    ("--name-analysis", Arg.Set name_analysis, "Perform name analysis");
    ("--type-check", Arg.Set type_check, "Perform type checking")
  ] in
  
  let usage_msg = "Usage: cigrid [flags] <file>" in
  
  let set_file f =
    if !input_file = "" then
      input_file := f
    else
      (prerr_endline "Error: Multiple input files specified"; 
       prerr_endline usage_msg; 
       exit 1)
  in
  
  (* Parse command line arguments *)
  begin try
    Arg.parse_argv ~current:(ref 0) Sys.argv arg_spec set_file usage_msg
  with
  | Arg.Bad msg -> 
      prerr_endline msg;
      exit 1
  | Arg.Help msg ->
      print_endline msg;
      exit 0
  end;

  if !input_file = "" then
    (prerr_endline "Error: No input file specified"; 
     prerr_endline usage_msg; 
     exit 1);

  (!input_file, !pretty_print, !line_error, !generate_asm, !liveness, !name_analysis, !type_check)

let handle_error (lexbuf: Lexing.lexbuf) (err_msg: string) (line_error_mode: bool) =
  let pos = lexbuf.lex_curr_p in
  if line_error_mode then
    prerr_endline (string_of_int pos.pos_lnum)
  else
    prerr_endline (Printf.sprintf
      "Error at %s, line %d, column %d: %s"
      pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) err_msg);
  exit 1

(* Semantic check: collect all defined struct names *)
let collect_struct_names (prog: global_def list) : string list =
  List.fold_left (fun acc g ->
    match g with
    | GStructDef (name, _) -> name :: acc
    | _ -> acc
  ) [] prog

(* Check if a type references only defined structs *)
let rec check_type (defined_structs: string list) (t: ty) : bool =
  match t with
  | TInt | TChar | TVoid -> true
  | TPtr inner -> check_type defined_structs inner
  | TArray (inner, _) -> check_type defined_structs inner
  | TStruct name -> List.mem name defined_structs
  | TIdent name -> List.mem name defined_structs

(* Check all types in an expression *)
let rec check_expr (defined_structs: string list) (e: expr) : bool =
  match e with
  | EInt _ | EChar _ | EString _ | EVar _ -> true
  | EBinOp (_, e1, e2) -> check_expr defined_structs e1 && check_expr defined_structs e2
  | EUnOp (_, e) -> check_expr defined_structs e
  | ECall (_, args) -> List.for_all (check_expr defined_structs) args
  | EParen e -> check_expr defined_structs e
  | ENew (t, e) -> check_type defined_structs t && check_expr defined_structs e
  | EArrayAccess (_, idx, _) -> check_expr defined_structs idx
  | EFieldAccess (e, _) -> check_expr defined_structs e
  | EAt (e, _) -> check_expr defined_structs e

(* Check all types in a statement *)
let rec check_stmt (defined_structs: string list) (s: stmt) : bool =
  match s with
  | SExpr e -> check_expr defined_structs e
  | SReturn e_opt -> 
      (match e_opt with None -> true | Some e -> check_expr defined_structs e)
  | SIf (cond, then_s, else_opt) ->
      check_expr defined_structs cond && check_stmt defined_structs then_s &&
      (match else_opt with None -> true | Some s -> check_stmt defined_structs s)
  | SWhile (cond, body) -> check_expr defined_structs cond && check_stmt defined_structs body
  | SFor (init_opt, cond_opt, update_opt, body) ->
      (match init_opt with None -> true | Some s -> check_stmt defined_structs s) &&
      (match cond_opt with None -> true | Some e -> check_expr defined_structs e) &&
      (match update_opt with None -> true | Some s -> check_stmt defined_structs s) &&
      check_stmt defined_structs body
  | SBreak -> true
  | SBlock stmts -> List.for_all (check_stmt defined_structs) stmts
  | SVarDef (t, _, e_opt) ->
      check_type defined_structs t &&
      (match e_opt with None -> true | Some e -> check_expr defined_structs e)
  | SAssign (_, e) -> check_expr defined_structs e
  | SArrayAssign (_, e_index, _, e_value) -> 
      check_expr defined_structs e_index && check_expr defined_structs e_value
  | SFieldAssign (_, _, e) -> check_expr defined_structs e
  | SDelete _ -> true

(* Check all types in a global definition *)
let check_global (defined_structs: string list) (g: global_def) : bool =
  match g with
  | GFuncDef (ret_t, _, params, body) ->
      check_type defined_structs ret_t &&
      List.for_all (fun (t, _) -> check_type defined_structs t) params &&
      check_stmt defined_structs body
  | GFuncDecl (ret_t, _, params) ->
      check_type defined_structs ret_t &&
      List.for_all (fun (t, _) -> check_type defined_structs t) params
  | GVarDef (t, _, e_opt) ->
      check_type defined_structs t &&
      (match e_opt with None -> true | Some e -> check_expr defined_structs e)
  | GVarDecl (t, _) -> check_type defined_structs t
  | GStructDef (_, fields) ->
      List.for_all (fun (t, _) -> check_type defined_structs t) fields

(* Check entire program for semantic errors *)
let check_program (prog: global_def list) : bool =
  let defined_structs = collect_struct_names prog in
  List.for_all (check_global defined_structs) prog

let () =
  let (filename, should_pretty_print, line_error_mode, generate_asm, do_liveness, do_name_analysis, do_type_check) = parse_args () in

  (* Load source lines for heuristic line reporting *)
  let file_lines =
    let ic = open_in filename in
    let rec loop acc =
      try
        let l = input_line ic in
        loop (l :: acc)
      with End_of_file ->
        close_in ic;
        Array.of_list (List.rev acc)
    in
    loop []
  in
  if line_error_mode then Semantic.set_source_lines file_lines else Semantic.set_source_lines [||];
  
  let in_channel = 
    try open_in filename
    with Sys_error msg -> 
      prerr_endline ("Cannot open file: " ^ msg);
      exit 1
  in

  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in in_channel;

    (* Extract global definitions from Program *)
    let Program global_defs = ast in

    (* Perform name analysis if requested *)
    if do_name_analysis then (
      try
        Semantic.analyze_names global_defs;
        exit 0
      with
      | Semantic.NameError (msg, line) ->
          if line_error_mode && line > 0 then
            prerr_endline (string_of_int line)
          else
            prerr_endline ("Name error: " ^ msg);
          exit 2
      | Semantic.SemanticError (msg, line) ->
          if line_error_mode && line > 0 then
            prerr_endline (string_of_int line)
          else if line_error_mode then
            prerr_endline "2"
          else
            prerr_endline ("Semantic error: " ^ msg);
          exit 2
    );

    (* Perform type checking if requested *)
    if do_type_check then (
      try
        Semantic.type_check global_defs;
        exit 0
      with
      | Semantic.TypeError (msg, line) ->
          if line_error_mode then
            (match (if line > 0 then Some line else Semantic.guess_error_line msg) with
             | Some l -> prerr_endline (string_of_int l)
             | None -> ())
          else
            prerr_endline ("Type error: " ^ msg);
          exit 2
      | Semantic.NameError (msg, line) ->
          if line_error_mode then
            (match (if line > 0 then Some line else Semantic.guess_error_line msg) with
             | Some l -> prerr_endline (string_of_int l)
             | None -> ())
          else
            prerr_endline ("Name error: " ^ msg);
          exit 2
      | Semantic.SemanticError (msg, line) ->
          if line_error_mode then
            (match (if line > 0 then Some line else Semantic.guess_error_line msg) with
             | Some l -> prerr_endline (string_of_int l)
             | None -> ())
          else
            prerr_endline ("Semantic error: " ^ msg);
          exit 2
    );

    if should_pretty_print then
      print_endline (string_of_program ast);
    
    if generate_asm then (
      let instrs = Compile.compile_program ast in
      List.iter (fun i -> print_endline (Asm.string_of_instr i)) instrs
    );

    if do_liveness then (
      let instrs = Compile.compile_program ast in
      Liveness.analyze_and_print instrs
    );

    exit 0

  with
  | Lexer.LexerError msg ->
      handle_error lexbuf ("Lexer error: " ^ msg) line_error_mode
  | Parsing.Parse_error ->
      handle_error lexbuf "Parse error" line_error_mode
  | Sys_error msg ->
      prerr_endline ("System error: " ^ msg);
      exit 1