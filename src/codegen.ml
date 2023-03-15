(* codegen.ml
   Translates a semantically-checked AST to LLVM IR.

   Written by: Team Nautilus (Ankur, Yuma, Max, Etha)
*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let translate (SProgram(statements)) = 
  let context    = L.global_context () in
  let i32_t      = L.i32_type    context (* TODO: if we use these types, can we update our LRM to say integers are 32-bits and no longer platform dependent? *)
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and quack_t    = L.void_type   context 
  and string_t   = L.pointer_type (L.i8_type context)
  and the_module = L.create_module context "sPool" in

  let ltype_of_typ = function
    A.Int    -> i32_t
  | A.Bool   -> i1_t
  | A.Float  -> float_t
  | A.Quack  -> quack_t
  | A.String -> string_t
  | _        -> raise (TODO "unimplemented ltype_of_typ")
  in
  
  let main_t                 = L.function_type i32_t [|  |] in
  let main_function          = L.define_function "main" main_t the_module in
  let builder                = L.builder_at_end context (L.entry_block main_function) in
  let int_format_str         = L.build_global_stringptr "%d\n" "fmt" builder in


  (* Declare all builtins *)
  let printf_t    = L.var_arg_function_type i32_t [| string_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let rec build_statement builder = function
    | _ -> raise (TODO "unimplemented statements in build_statement")
  and expr builder (t, e) = match e with 
    | _ -> raise (TODO "unimplemented other expressions in expr")
  and build_function builder (store, retty, formals, body) = raise (TODO "unimplemented build_function")
  and build_main_function builder statements =
    (* Generate instructions for the actual sPool source *)
    List.map (build_statement builder) statements; 
      
    (* End the main basic block with a terminal *)
    terminate_block builder A.Int
  and add_terminal builder instr =
    (match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder))
  and terminate_block builder retty = add_terminal builder (match retty with
    A.Quack -> L.build_ret_void
  | A.Float -> L.build_ret (L.const_float float_t 0.0)
  | A.Int   -> L.build_ret (L.const_int i32_t 0)
  | _ -> raise (TODO "unimplemented return types in terminate_block"))
in
(* We only have one top-level function, main. 
   All statements of the sPool program reside within main *)
let _ = build_main_function builder statements in the_module