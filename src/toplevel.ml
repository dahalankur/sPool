(* toplevel.ml
   The top level driver program for the sPool compiler.
   Allows the user to select which stage of the compiler to run, and
   which file to compile.
   Usage: ./toplevel.native [-a|-l|-s|-c] [file.sP]

   Written by: Team Nautilus (Ankur, Yuma, Max, Etha)
*)

type action = Ast | Sast | LLVM_IR | Compile | ImportStdlib

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let usage_msg = "Usage: ./toplevel.native [-a|-l|-s|-c] [file.sP]" in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
    ("-i", Arg.Unit (set_action ImportStdlib), "Import the standard library, and check and print the generated LLVM IR");] in
  let channel = ref stdin in
    Arg.parse speclist (fun file -> channel := open_in file) usage_msg; 
  let lexbuf = Lexing.from_channel !channel in
    match !action with
      Ast -> print_endline (Ast.ast_of_program (Parser.program Scanner.token lexbuf))
    | _   ->
        match !action with
          Sast    -> print_endline (Sast.sast_of_sprogram (Semant.check (Parser.program Scanner.token lexbuf)))
        | LLVM_IR -> print_endline (Llvm.string_of_llmodule (Codegen.translate (Semant.check (Parser.program Scanner.token lexbuf))))
        | Compile -> let m = Codegen.translate (Semant.check (Parser.program Scanner.token lexbuf)) in Llvm_analysis.assert_valid_module m;
                      print_endline (Llvm.string_of_llmodule m)
        | ImportStdlib -> 
          let stdlib_list = open_in "../stdlib/list.sP" in
          let stdlib_string = open_in "../stdlib/string.sP" in

          let temp_file = open_out "temp.sP" in
          
          (* copy contents of list.sP and string.sP to temp.sP *)
          let rec copy_file_to_temp_file file =
            try
              let line = input_line file in
              Printf.fprintf temp_file "%s\n" line;
              copy_file_to_temp_file file
            with End_of_file -> ()
          in
          copy_file_to_temp_file stdlib_list;
          copy_file_to_temp_file stdlib_string;
          
          (* copy contents of input file to temp.sP *)
          (* read contents of !channel into a variable *)
          let input_file_string = ref "" in
          let rec read_input_file file =
            try
              let line = input_line file in
              input_file_string := !input_file_string ^ line ^ "\n";
              read_input_file file
            with End_of_file -> ()
          in
          read_input_file !channel;
          (* write contents of input file to temp.sP *)
          Printf.fprintf temp_file "%s" !input_file_string;
          close_in !channel;
          close_in stdlib_list;
          close_in stdlib_string;
          close_out temp_file;

          let temp_lexbuf = Lexing.from_channel (open_in "temp.sP") in
          let m = Codegen.translate (Semant.check (Parser.program Scanner.token temp_lexbuf)) in Llvm_analysis.assert_valid_module m;
          print_endline (Llvm.string_of_llmodule m);

          (* delete temp.sP *)
          Sys.remove "temp.sP";

        | _ -> raise (Failure "Internal error: no action selected")