(* file: parser.mly *)

%{
  open Ast
  
  let op_of_token = function
    | PLUS -> Add | MINUS -> Sub | TIMES -> Mul | DIV -> Div | MOD -> Mod
    | EQ -> Eq | NEQ -> Neq | LT -> Lt | GT -> Gt | LE -> Le | GE -> Ge
    | AND -> And | OR -> Or | BAND -> BAnd | BOR -> BOr | BXOR -> BXor
    | SHL -> Shl | SHR -> Shr
    | _ -> failwith "Not a binary operator token"
    
  let unop_of_token = function
    | MINUS -> Neg | NOT -> Not | BNOT -> BNot
    | _ -> failwith "Not a unary operator token"

  let with_pos e pos = EAt(e, pos.Lexing.pos_lnum)
  
  (* Helper to create location from position *)
  let make_location pos = 
    { line = pos.Lexing.pos_lnum; column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol }
%}

%token <int> UINT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> IDENT

%token IF ELSE WHILE FOR RETURN BREAK EXTERN
%token NEW DELETE STRUCT
%token INT CHAR VOID

%token PLUSPLUS MINUSMINUS
%token PLUS MINUS TIMES DIV MOD
%token EQ NEQ LT GT LE GE
%token AND OR BAND BOR BXOR
%token NOT BNOT
%token SHL SHR
%token ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMI COMMA DOT
%token EOF

%right ASSIGN
%left OR
%left AND
%left BOR
%left BXOR
%left BAND
%left EQ NEQ
%left LT GT LE GE
%left SHL SHR
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT BNOT UMINUS

%start program
%type <Ast.program> program

%%

program:
  | global_defs EOF { Program($1) }
;

global_defs:
  | /* empty */ { [] }
  | global_defs global_def { $1 @ [$2] }
;

global_def:
  | ty IDENT LPAREN params RPAREN LBRACE stmts RBRACE
    { 
      let loc = make_location (Parsing.rhs_start_pos 6) in
      GFuncDef($1, $2, $4, SBlock($7, loc)) 
    }
  | EXTERN ty IDENT LPAREN params RPAREN SEMI
    { GFuncDecl($2, $3, $5) }
  | EXTERN ty IDENT SEMI
    { GVarDecl($2, $3) }
  | STRUCT IDENT LBRACE struct_fields RBRACE SEMI
    { GStructDef($2, $4) }
  | ty IDENT LBRACKET UINT RBRACKET SEMI
    { GVarDef(TArray($1, $4), $2, None) }
  | ty IDENT LBRACKET UINT RBRACKET ASSIGN expr SEMI
    { GVarDef(TArray($1, $4), $2, Some $7) }
  | ty IDENT SEMI
    { GVarDef($1, $2, None) }
  | ty IDENT ASSIGN expr SEMI
    { GVarDef($1, $2, Some $4) }
;

struct_fields:
  | /* empty */ { [] }
  | struct_fields ty IDENT SEMI { $1 @ [($2, $3)] }
;

params:
  | /* empty */ { [] }
  | VOID        { [] }
  | param_list  { $1 }
;
param_list:
  | ty IDENT { [($1, $2)] }
  | param_list COMMA ty IDENT { $1 @ [($3, $4)] }
;

ty:
  | base_ty { $1 }
  | ty TIMES { TPtr($1) }
;

base_ty:
  | INT  { TInt }
  | CHAR { TChar }
  | VOID { TVoid }
  | STRUCT IDENT { TStruct($2) }
  | IDENT { TIdent($1) }
;

stmts:
  | /* empty */ { [] }
  | stmts stmt  { $1 @ [$2] }
;

stmt:
  | expr SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SExpr($1, loc) 
    }
  | LBRACE stmts RBRACE 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SBlock($2, loc) 
    }
  | DELETE LBRACKET RBRACKET IDENT SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SDelete($4, loc) 
    }
  | RETURN SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SReturn(None, loc) 
    }
  | RETURN expr SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SReturn(Some $2, loc) 
    }
  | IF LPAREN expr RPAREN stmt
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SIf($3, $5, None, loc) 
    }
  | IF LPAREN expr RPAREN stmt ELSE stmt
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SIf($3, $5, Some $7, loc) 
    }
  | WHILE LPAREN expr RPAREN stmt
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SWhile($3, $5, loc) 
    }
  | FOR LPAREN for_init for_cond for_update RPAREN stmt
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      let init_loc = make_location (Parsing.rhs_start_pos 3) in
      let init_stmts = match $3 with None -> [] | Some s -> [s] in
      let cond = match $4 with None -> EInt(1) | Some e -> e in
      let update_stmt = match $5 with None -> [] | Some s -> [s] in
      let body_stmts = [$7] @ update_stmt in
      let while_loop = SWhile(cond, SBlock(body_stmts, loc), loc) in
      SBlock(init_stmts @ [while_loop], loc)
    }
  | BREAK SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SBreak(loc) 
    }
  | ty IDENT LBRACKET UINT RBRACKET SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SVarDef(TArray($1, $4), $2, None, loc) 
    }
  | ty IDENT LBRACKET UINT RBRACKET ASSIGN expr SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SVarDef(TArray($1, $4), $2, Some $7, loc) 
    }
  | ty IDENT SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SVarDef($1, $2, None, loc) 
    }
  | ty IDENT ASSIGN expr SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SVarDef($1, $2, Some $4, loc) 
    }
  | IDENT LBRACKET expr RBRACKET DOT IDENT ASSIGN expr SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SArrayAssign($1, $3, Some $6, $8, loc) 
    }
  | IDENT LBRACKET expr RBRACKET ASSIGN expr SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SArrayAssign($1, $3, None, $6, loc) 
    }
  | IDENT LBRACKET expr RBRACKET DOT IDENT PLUSPLUS SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SArrayAssign($1, $3, Some $6, EBinOp(Add, EArrayAccess($1, $3, Some $6), EInt(1)), loc) 
    }
  | IDENT LBRACKET expr RBRACKET DOT IDENT MINUSMINUS SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SArrayAssign($1, $3, Some $6, EBinOp(Sub, EArrayAccess($1, $3, Some $6), EInt(1)), loc) 
    }
  | IDENT LBRACKET expr RBRACKET PLUSPLUS SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SArrayAssign($1, $3, None, EBinOp(Add, EArrayAccess($1, $3, None), EInt(1)), loc) 
    }
  | IDENT LBRACKET expr RBRACKET MINUSMINUS SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SArrayAssign($1, $3, None, EBinOp(Sub, EArrayAccess($1, $3, None), EInt(1)), loc) 
    }
  | IDENT ASSIGN expr SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SAssign($1, $3, loc) 
    }
  | IDENT PLUSPLUS SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SAssign($1, EBinOp(Add, EVar($1), EInt(1)), loc) 
    }
  | IDENT MINUSMINUS SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SAssign($1, EBinOp(Sub, EVar($1), EInt(1)), loc) 
    }
  | PLUSPLUS IDENT SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SAssign($2, EBinOp(Add, EVar($2), EInt(1)), loc) 
    }
  | MINUSMINUS IDENT SEMI
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      SAssign($2, EBinOp(Sub, EVar($2), EInt(1)), loc) 
    }
;

(* For loop components *)
for_init:
  | SEMI 
    { None }
  | ty IDENT SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SVarDef($1, $2, None, loc)) 
    }
  | ty IDENT ASSIGN expr SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SVarDef($1, $2, Some $4, loc)) 
    }
  | IDENT ASSIGN expr SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SAssign($1, $3, loc)) 
    }
  | expr SEMI 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SExpr($1, loc)) 
    }
;

for_cond:
  | SEMI { None }
  | expr SEMI { Some $1 }
;

for_update:
  | /* empty */ { None }
  | IDENT ASSIGN expr 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SAssign($1, $3, loc)) 
    }
  | IDENT PLUSPLUS 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SAssign($1, EBinOp(Add, EVar($1), EInt(1)), loc)) 
    }
  | IDENT MINUSMINUS 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SAssign($1, EBinOp(Sub, EVar($1), EInt(1)), loc)) 
    }
  | PLUSPLUS IDENT 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SAssign($2, EBinOp(Add, EVar($2), EInt(1)), loc)) 
    }
  | MINUSMINUS IDENT 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SAssign($2, EBinOp(Sub, EVar($2), EInt(1)), loc)) 
    }
  | expr 
    { 
      let loc = make_location (Parsing.rhs_start_pos 1) in
      Some (SExpr($1, loc)) 
    }
;

expr:
  | UINT { with_pos (EInt($1)) (Parsing.rhs_start_pos 1) }
  | CHAR_LIT { with_pos (EChar($1)) (Parsing.rhs_start_pos 1) }
  | STRING_LIT { with_pos (EString($1)) (Parsing.rhs_start_pos 1) }
  | IDENT { with_pos (EVar($1)) (Parsing.rhs_start_pos 1) }
  | IDENT LPAREN args RPAREN
    { with_pos (ECall($1, $3)) (Parsing.rhs_start_pos 1) }
  | IDENT LBRACKET expr RBRACKET
    { with_pos (EArrayAccess($1, $3, None)) (Parsing.rhs_start_pos 1) }
  | IDENT LBRACKET expr RBRACKET DOT IDENT
    { with_pos (EArrayAccess($1, $3, Some $6)) (Parsing.rhs_start_pos 1) }
  | LPAREN expr RPAREN
    { with_pos (EParen($2)) (Parsing.rhs_start_pos 1) }
  | NEW ty LBRACKET expr RBRACKET { with_pos (ENew($2, $4)) (Parsing.rhs_start_pos 1) }
  | MINUS expr %prec UMINUS { with_pos (EUnOp(Neg, $2)) (Parsing.rhs_start_pos 1) }
  | NOT expr   { with_pos (EUnOp(Not, $2)) (Parsing.rhs_start_pos 1) }
  | BNOT expr  { with_pos (EUnOp(BNot, $2)) (Parsing.rhs_start_pos 1) }
  | expr PLUS expr  { with_pos (EBinOp(Add, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr MINUS expr { with_pos (EBinOp(Sub, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr TIMES expr { with_pos (EBinOp(Mul, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr DIV expr   { with_pos (EBinOp(Div, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr MOD expr   { with_pos (EBinOp(Mod, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr EQ expr    { with_pos (EBinOp(Eq, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr NEQ expr   { with_pos (EBinOp(Neq, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr LT expr    { with_pos (EBinOp(Lt, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr GT expr    { with_pos (EBinOp(Gt, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr LE expr    { with_pos (EBinOp(Le, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr GE expr    { with_pos (EBinOp(Ge, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr AND expr   { with_pos (EBinOp(And, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr OR expr    { with_pos (EBinOp(Or, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr BAND expr  { with_pos (EBinOp(BAnd, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr BOR expr   { with_pos (EBinOp(BOr, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr BXOR expr  { with_pos (EBinOp(BXor, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr SHL expr   { with_pos (EBinOp(Shl, $1, $3)) (Parsing.rhs_start_pos 1) }
  | expr SHR expr   { with_pos (EBinOp(Shr, $1, $3)) (Parsing.rhs_start_pos 1) }
;

args:
  | /* empty */ { [] }
  | expr_list   { $1 }
;
expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }
;
%%