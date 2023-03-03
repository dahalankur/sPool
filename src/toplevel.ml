open Ast
open Semant

let () =
  let usage_msg = "Usage: ./toplevel.native [-testparser|-testsemant] [file]" in
  let channel = ref stdin in
  let testparser = ref false in (* default cmd args get set to false *)
  let testsemant = ref false in
  let speclist =
    [("-testparser", Arg.Set testparser, "Test scanner and parser");
     ("-testsemant", Arg.Set testsemant, "Test semantic checker")] in
    Arg.parse speclist (fun file -> channel := open_in file) usage_msg;
  let lex_buf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lex_buf in
  let sast = check ast in
    (* Print ast if -testparser is given; if -testsemant is given, print sast *)
    if !testparser then print_endline (ast_of_program ast)
    else if !testsemant then print_endline (sast_of_sprogram sast)
    else print_endline usage_msg