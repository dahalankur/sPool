{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
    [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
  | "#"     { comment lexbuf } 
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '['      { LSQUARE }
  | ']'      { RSQUARE }
  | ':'      { COLON  }
  | ';'      { SEMI   }
  | ','      { COMMA }
  | "->"     { ARROW }
  | "return" { RETURN }
  | "int"    { INT }
  | "list"   { LIST }
  | "bool"   { BOOL }
  | "string" { STRING }
  | "float"  { FLOAT }
  | "quack"  { QUACK } 
  | "thread" { THREAD }
  | "mutex"  { MUTEX }
  | "false"  { BLIT(false) }
  | "true"   { BLIT(true)  } 
  | "def"    { DEF }
  | "lambda" { LAMBDA }
  | "store"  { STORE }
  | '"'(("\\"_)|[^'\n' '"' '\\'])*'"'  as str { STRINGLIT(str) }
  | "if"     { IF  }
  | "else"   { ELSE } 
  | "while"  { WHILE }
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
  | digits '.'  digit* as lxm { FLIT(lxm) } 
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { NAME(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  | '\n' { NEWLINE }
  | eof  { EOF }
  | _    { comment lexbuf }
