{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
    [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
  | "#"     { comment lexbuf } 
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | ':'      { COLON  }
  | ';'      { SEMI   }
  (* 
 
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ';'      { SEMI }
  | ','      { COMMA } *)
  (* | "for"    { FOR }
  | "while"  { WHILE }
  | "return" { RETURN }
  | "int"    { INT }
  | "bool"   { BOOL }
  | "float"  { FLOAT }
  | "void"   { VOID }
  | "true"   { BLIT(true)  }
  | "false"  { BLIT(false) } *)
  | "if"     { IF  }
  | "else"   { ELSE } 
  | '\n'     { NEWLINE }
  | '='      { ASSIGN }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "&&"     { AND }
  | "||"     { OR }
  | "!"      { NOT }
  | "%"      { MOD }
  | digits as lxm { LITERAL(int_of_string lxm) }
  (* | digits '.'  digit* as lxm { FLIT(lxm) } *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { NAME(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  | '\n' { NEWLINE }
  | _    { comment lexbuf }
