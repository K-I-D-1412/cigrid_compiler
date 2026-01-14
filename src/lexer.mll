(* file: lexer.mll *)

{
  open Parser
  
  exception LexerError of string
  
  let unescape_char c = match c with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | 'b' -> '\b'
    | '0' -> '\000'
    | '\\' -> '\\'
    | '\'' -> '\''
    | '"' -> '\"'
    | _ -> raise (LexerError (Printf.sprintf "Invalid escape sequence: \\\\%c" c))
  
  (* Process escape sequences in strings *)
  let unescape_string s =
    let buf = Buffer.create (String.length s) in
    let rec process i =
      if i >= String.length s then
        Buffer.contents buf
      else if s.[i] = '\\' && i + 1 < String.length s then (
        Buffer.add_char buf (unescape_char s.[i+1]);
        process (i + 2)
      ) else (
        Buffer.add_char buf s.[i];
        process (i + 1)
      )
    in
    process 0
}

rule token = parse
  | [' ' '\t' '\r'] { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  
  (* Comments: C-style and '#' line comments *)
  | "//" [^ '\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | "#"  [^ '\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "#"  [^ '\n']* { token lexbuf }
  | "/*"           { comment lexbuf }

  (* Keywords *)
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "for"    { FOR }
  | "return" { RETURN }
  | "int"    { INT }
  | "char"   { CHAR }
  | "void"   { VOID }
  | "break"  { BREAK }
  | "extern" { EXTERN }
  | "new"    { NEW }
  | "delete" { DELETE }
  | "struct" { STRUCT }

  (* Operators - extended for G-level *)
  | "++"  { PLUSPLUS }
  | "--"  { MINUSMINUS }
  | "<<"  { SHL }
  | ">>"  { SHR }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIV }
  | "%"  { MOD }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<"  { LT }
  | ">"  { GT }
  | "&&" { AND }
  | "||" { OR }
  | "&"  { BAND }
  | "|"  { BOR }
  | "^"  { BXOR }
  | "!"  { NOT }
  | "~"  { BNOT }
  | "="  { ASSIGN }

  (* Punctuation *)
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | ";"  { SEMI }
  | ","  { COMMA }
  | "."  { DOT }

  (* Character literals with escape sequences *)
  (* Only allow printable ASCII characters (excluding newline, tab, etc.) *)
  (* Reject unescaped double quote *)
  | '\'' '\\' (_ as c) '\'' { CHAR_LIT (unescape_char c) }
  | '\'' ([^ '\\' '\'' '\n' '\r' '\t' '\b' '"'] as c) '\'' { CHAR_LIT (c) }
  
  (* String literals - must come before character check *)
  | '"' { string_literal (Buffer.create 16) lexbuf }
  
  (* Integer literals - support decimal and hexadecimal *)
  | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ as lxm { UINT (int_of_string lxm) }
  | "0X" ['0'-'9' 'a'-'f' 'A'-'F']+ as lxm { UINT (int_of_string lxm) }
  | '0' { UINT 0 }
  | ['1'-'9'] ['0'-'9']* as lxm { UINT (int_of_string lxm) }
  
  (* Identifiers *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { IDENT (lxm) }

  (* End of file *)
  | eof { EOF }

  (* Error *)
  | _ as c { raise (LexerError (Printf.sprintf "Unknown token: %c" c)) }

and comment = parse
  | "*/" { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (LexerError "Unclosed comment") }

and string_literal buf = parse
  | '"'  { STRING_LIT (Buffer.contents buf) }
  | '\\' (_ as c)  { 
      (* Validate escape sequence *)
      let escaped_char = 
        try unescape_char c 
        with LexerError _ -> raise (LexerError (Printf.sprintf "Invalid escape sequence: \\%c" c))
      in
      Buffer.add_char buf escaped_char;
      string_literal buf lexbuf 
    }
  | '\n' { raise (LexerError "Unescaped newline in string literal") }
  | '\t' { raise (LexerError "Unescaped tab in string literal") }
  | '\r' { raise (LexerError "Unescaped carriage return in string literal") }
  | '\'' { raise (LexerError "Unescaped single quote in string literal") }
  | eof  { raise (LexerError "Unterminated string literal") }
  | _ as c { 
      Buffer.add_char buf c;
      string_literal buf lexbuf 
    }