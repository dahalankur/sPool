open Ast

let () =
  let lex_buf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lex_buf in
  print_endline (ast_of_program program)