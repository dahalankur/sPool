(* toplevel.ml
   The top level driver program for the sPool compiler.
   Allows the user to select which stage of the compiler to run, and
   which file to compile.
   Usage: ./toplevel.native [-a|-l|-s|-c] [file.sP]

   Written by: Team Nautilus (Ankur, Yuma, Max, Etha)
*)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let usage_msg = "Usage: ./toplevel.native [-a|-l|-s|-c] [file.sP]" in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)")] in
  let channel = ref stdin in
    Arg.parse speclist (fun file -> channel := open_in file) usage_msg; 
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
    match !action with
      Ast -> print_endline (Ast.ast_of_program ast)
    | _   -> let sast = Semant.check ast in
        match !action with
          Sast    -> print_endline (Sast.sast_of_sprogram sast)
        | LLVM_IR -> print_endline (Llvm.string_of_llmodule (Codegen.translate sast))
        | Compile -> let m = Codegen.translate sast in Llvm_analysis.assert_valid_module m;
                      print_endline (Llvm.string_of_llmodule m)
        | _ -> raise (Failure "Internal error: no action selected")