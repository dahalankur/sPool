open Ast

(* module StringMap = Map.Make(String) *)

(* let rec eval map = 
function 
  |  *)
(* Test with binop nested within assignment to see what behavior we get with map'' being returned.... *)
let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lex_buf in
  (* let (result, _) = eval StringMap.empty expr in *)
  print_endline (ast_of_expr expr)

