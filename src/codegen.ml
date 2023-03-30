(* codegen.ml
   Translates a semantically-checked AST to LLVM IR.

   Written by: Team Nautilus (Ankur, Yuma, Max, Etha)
*)

module L = Llvm
module A = Ast
module S = Stack
open Sast 

module StringMap = Map.Make(String)

let context    = L.global_context ()
let i32_t      = L.i32_type    context (* TODO: if we use these types, can we update our LRM to say integers are 32-bits and no longer platform dependent? *)
and i8_t       = L.i8_type     context
and i1_t       = L.i1_type     context
and float_t    = L.double_type context (* TODO: also update this info in LRM about the internal representation of floating point numbers *)
and quack_t    = L.void_type   context 
and string_t   = L.pointer_type (L.i8_type context) 
and voidptr_t  = L.pointer_type (L.i8_type context)

let ltype_of_typ = function
  A.Int    -> i32_t
| A.Bool   -> i1_t
| A.Float  -> float_t
| A.Quack  -> quack_t
| A.String -> string_t
| _        -> raise (TODO "unimplemented ltype_of_typ")

type symbol_table = {
  variables : L.llvalue StringMap.t;
  shared : bool StringMap.t;
  parent : symbol_table option;
}

let rec find_variable (scope : symbol_table) name =
  try
    StringMap.find name scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _            -> raise (Failure ("Internal Error: should have been caught in semantic analysis"))

let rec find_shared (scope : symbol_table) name = 
  try
    StringMap.find name scope.shared
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_shared parent name
    | _            -> raise (Failure ("Internal Error: should have been caught in semantic analysis"))

(* initial env *)
let env : symbol_table ref = ref { variables = StringMap.empty; shared = StringMap.empty; parent = None }

let add_to_scope (s, v, n) builder t =
  (* TODO: 
  
    generate different instructions based on if the variable is shared or not
    when not shared, this should suffice.
    when shared, generate alloc (stack -> allocate a pointer to the supplied type t), malloc (allocate space for the actual type on the heap using build_malloc), 
    store (the value on the heap), store (the address of the heap-allocated object on the stack). Need to think about updating the scope -> should 
    the scope be updated with the address on the stack or the address on the heap? I think it should be the address on the heap since that is what
    we will be using to access the variable (i.e. dereferencing the pointer on the stack) but we should think more about this and actually look at the
    C code that does this and what LLVM it spits out. We should accordingly handle assignment and svar (in general) and formals in functions. nice!

    NOTE: this was found by looking at the generated code for a c program that does the same thing.
    *)

  if not s then
    let local = L.build_alloca (ltype_of_typ t) n builder in
    let _     = L.build_store v local builder in
    let new_scope = {variables = StringMap.add n local !env.variables; shared = StringMap.add n s !env.shared; parent = !env.parent}
      in env := new_scope 
  else
    let local = L.build_alloca (L.pointer_type (ltype_of_typ t)) n builder in
    let heap  = L.build_malloc (ltype_of_typ t) n builder in
    let _     = L.build_store heap local builder in
    let _     = L.build_store v heap builder in

    let new_scope = {variables = StringMap.add n heap !env.variables; shared = StringMap.add n s !env.shared; parent = !env.parent}
      in env := new_scope

let push_scope () = 
  let new_scope = {variables = StringMap.empty; shared = StringMap.empty; parent = Some(!env)}
  in env := new_scope

let pop_scope () = 
  let parent_scope = match !env.parent with 
      Some(parent) -> parent
    | _            -> raise (Failure "Internal Error: should not happen in pop_scope; should have been caught in semantic analysis")
  in env := parent_scope

let translate (SProgram(statements)) = 
  let the_module = L.create_module context "sPool" in

  let main_t                 = L.function_type i32_t [|  |] in
  let main_function          = L.define_function "main" main_t the_module in
  let builder                = L.builder_at_end context (L.entry_block main_function) in
  let str_format_str         = L.build_global_stringptr "%s"   "strfmt"        builder in
  let str_format_str_endline = L.build_global_stringptr "%s\n" "strfmtendline" builder in
  

  (* ----------- beginning of stack-related bookkeeping functions ----------- 
    
     These stack-related functions are only for bookkeeping purposes.
     Specfically, when generating branching instructions (If and While), 
     we need to know which function we are inside at that given point since 
     we need to create new blocks like "merge", "then", "else", etc.
     For this, the the_function function is useful since it returns the 
     function definition for which the code is being generated at any 
     given point. To maintain this invariant, whenever we see a new function 
     being created (LAMBDA in our case), we need to ensure that its 
     definition is pushed to the stack and then only its body is generated.
     The stack must be popped once we are done generating instructions for 
     that function body.   
  *)
  
  let curr_function     = ref (S.create ()) in 
  let the_function   () = S.top      !curr_function in
  let push_function   f = S.push f   !curr_function in
  let pop_function   () = S.pop      !curr_function in
  let is_stack_empty () = S.is_empty !curr_function in
  
  (* ----------- end of stack-related bookkeeping functions ----------- *)


  (* ----- start of builtin function declarations ----- *)

  let printf_t             = L.var_arg_function_type i32_t [| string_t |] in
  let printf_func          = L.declare_function "printf" printf_t the_module in

  let int_to_string_t      = L.function_type string_t [| i32_t |] in
  let int_to_string_func   = L.declare_function "int_to_string" int_to_string_t the_module in
  
  let float_to_string_t    = L.function_type string_t [| float_t |] in
  let float_to_string_func = L.declare_function "float_to_string" float_to_string_t the_module in

  let bool_to_string_t     = L.function_type string_t [| i1_t |] in
  let bool_to_string_func  = L.declare_function "bool_to_string" bool_to_string_t the_module in

  let int_to_float_t       = L.function_type float_t [| i32_t |] in
  let int_to_float_func    = L.declare_function "int_to_float" int_to_float_t the_module in (* TODO: in lrm, talk about how types are being cast and what precision/accuracy can be lost *)

  let float_to_int_t       = L.function_type i32_t [| float_t |] in
  let float_to_int_func    = L.declare_function "float_to_int" float_to_int_t the_module in (* TODO: in lrm, talk about how types are being cast and what precision/accuracy can be lost *)

  let strlen_t             = L.function_type i32_t [| string_t |] in
  let strlen_func          = L.declare_function "strlen" strlen_t the_module in

  let string_concat_t     = L.function_type string_t [| string_t; string_t |] in
  let string_concat_func  = L.declare_function "string_concat" string_concat_t the_module in

  let string_substr_t     = L.function_type string_t [| string_t; i32_t; i32_t |] in
  let string_substr_func  = L.declare_function "string_substr" string_substr_t the_module in

  let string_eq_t        = L.function_type i1_t [| string_t; string_t |] in
  let string_eq_func     = L.declare_function "string_eq" string_eq_t the_module in

  (* ----- end of builtin function declarations ----- *)

  (* TODO: handling shared vars note:
    for shared variables passed to functions as arguments, we need to first evaluate the variable and then pass it to the function
    to maintain our convention of pass-by-value for all nonlists and nonmutexes.
    
    For lists and mutexes, however, this is not the case. They are shared by default, so their addresses are passed in when calling 
    functions! This means that we need to pass the address of the variable to the function, not the value of the variable.

    SHARED VARIABLES ARE DECLARED ON THE HEAP! 
  *)

  let rec statement builder = function
      SExpr e -> let _     = expr builder e in builder
    | SIf (predicate, then_stmt, else_stmt) ->
        let the_function   = the_function () in

        let bool_val       = expr builder predicate in
        
        let _ = push_scope () in

        let merge_bb       = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in
        
        let then_bb        = L.append_block context "then" the_function in
          let then_builder = List.fold_left statement (L.builder_at_end context then_bb) then_stmt in
        let ()             = add_terminal then_builder branch_instr in
        
        let _ = pop_scope() in
        let _ = push_scope () in
        
        let else_bb        = L.append_block context "else" the_function in
          let else_builder = List.fold_left statement (L.builder_at_end context else_bb) else_stmt in
        let ()             = add_terminal else_builder branch_instr in
        
        let _ = pop_scope() in
        
        let _              = L.build_cond_br bool_val then_bb else_bb builder in L.builder_at_end context merge_bb
    | SWhile (predicate, body) ->
        let the_function      = the_function () in
        
        let pred_bb           = L.append_block context "while" the_function in
        let _                 = L.build_br pred_bb builder in
        
        let _ = push_scope() in
        
        let body_bb           = L.append_block context "while_body" the_function in
            let while_builder = List.fold_left statement (L.builder_at_end context body_bb) body in
        let ()                = add_terminal while_builder (L.build_br pred_bb) in

        let pred_builder      = L.builder_at_end context pred_bb in
        let bool_val          = expr pred_builder predicate in

        let _ = pop_scope() in (* TODO: test with a loop that has variable definitions inside of it.... it should be redefined every time *)

        let merge_bb          = L.append_block context "merge" the_function in
        let _                 = L.build_cond_br bool_val body_bb merge_bb pred_builder in L.builder_at_end context merge_bb
    | SAssign (name, e) -> 
        let e' = expr builder e in 
        let _  = L.build_store e' (find_variable !env name) builder in builder
    | SDefine(s, typ, name, e) -> 
        let e' = expr builder e in
        let _  = add_to_scope (s, e', name) builder typ in builder
    | _ -> raise (TODO "unimplemented statements in statement")
  and expr builder (t, e) = match e with 
      SLiteral i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SFliteral l -> L.const_float_of_string float_t l
    | SVar (s, name) -> L.build_load (find_variable !env name) name builder
    (*| SVar (true, name) -> raise (TODO "shared variables not implemented yet")*)
    | SNoexpr -> L.const_int i32_t 0
    | SStringLiteral s -> L.build_global_stringptr s "strlit" builder
    | SCall ("print", [e])   -> L.build_call printf_func [| str_format_str ; (expr builder e) |] "printf" builder
    | SCall ("println", [e]) -> L.build_call printf_func [| str_format_str_endline ; (expr builder e) |] "printf" builder
    | SCall ("int_to_string", [e]) -> L.build_call int_to_string_func [| (expr builder e) |] "int_to_string" builder
    | SCall ("float_to_string", [e]) -> L.build_call float_to_string_func [| (expr builder e) |] "float_to_string" builder
    | SCall ("bool_to_string", [e]) -> L.build_call bool_to_string_func [| (expr builder e) |] "bool_to_string" builder
    | SCall ("int_to_float", [e]) -> L.build_call int_to_float_func [| (expr builder e) |] "int_to_float" builder
    | SCall ("float_to_int", [e]) -> L.build_call float_to_int_func [| (expr builder e) |] "float_to_int" builder
    | SCall ("String_eq", [e1; e2]) -> L.build_call string_eq_func [| (expr builder e1); (expr builder e2) |] "string_eq" builder
    | SCall ("String_len", [e]) -> L.build_call strlen_func [| (expr builder e) |] "strlen" builder
    | SCall ("String_concat", [e1; e2]) -> L.build_call string_concat_func [| (expr builder e1); (expr builder e2) |] "string_concat" builder
    | SCall ("String_substr", [e1; e2; e3]) -> L.build_call string_substr_func [| (expr builder e1); (expr builder e2); (expr builder e3) |] "string_substr" builder
    | SCall(f, args) -> raise (TODO "unimplemented function calls in expr")
    | SBinop (e1, op, e2) ->
      let (t, _) = e1
      and e1' = expr builder e1
      and e2' = expr builder e2 in
      if t = A.Float (* TODO: double check this matches our semantics for binops and unop from the LRM *)
      then (match op with 
        A.Add     -> L.build_fadd
      | A.Sub     -> L.build_fsub
      | A.Mult    -> L.build_fmul
      | A.Div     -> L.build_fdiv 
      | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
      | A.Neq     -> L.build_fcmp L.Fcmp.One
      | A.Less    -> L.build_fcmp L.Fcmp.Olt
      | A.Leq     -> L.build_fcmp L.Fcmp.Ole
      | A.Greater -> L.build_fcmp L.Fcmp.Ogt
      | A.Geq     -> L.build_fcmp L.Fcmp.Oge
      | A.Mod        -> raise (Failure "Internal Error: semant should have rejected mod on float")
      | A.And | A.Or -> raise (Failure "internal Error: semant should have rejected and/or on float")
           ) e1' e2' "tmp" builder 
      else (match op with              (* TODO: what about binary operators on data types other than Int or Floats? Do we handle everything properly here? Semant should be solidified in order to reject stuff as specified in the LRM *)
      | A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.Mod     -> L.build_srem (* TODO: srem or urem? mention in LRM that the srem is a tad bit different than traditional mod operations. see LLVM docs for more info *)
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
           ) e1' e2' "tmp" builder
    | SUnop(op, e) ->
      let (t, _) = e in
      let e' = expr builder e in
      (match op with
          A.Neg when t = A.Float -> L.build_fneg 
        | A.Neg                  -> L.build_neg
        | A.Not                  -> L.build_not 
      ) e' "tmp" builder
    | _ -> raise (TODO "unimplemented other expressions in expr")
  and build_function builder (store, retty, formals, body) = () (* TODO: raise (TODO "unimplemented build_function") *)
  and build_main_function builder statements =
    (* Note to self: at this point, final_builder is pointing to the END of the main function. 
       The call to statement generates instructions for all statments in this main function, 
       which subsequently keeps on updating the instruction builder. Therefore, after the last 
       instruction in main's body is generated, the builder points to that instruction and 
       this is stored in final_builder. This is different that the `builder` in the argument since that 
       builder is still pointing to the beginning of the main function! *)
    
    (* Push main_function definition to the curr_function stack first *)
    let _ = push_function main_function in

    let final_builder = List.fold_left statement builder statements in  (* TODO: every time we build statements, remember to fold to get the updated builder after that statement's instruction! *)
    
    (* End the main function's basic block with a terminal *)
    let _ = terminate_block final_builder A.Int in
    
    (* Pop main_function definition from the curr_function stack *)
    let _ = pop_function () in
      if (not (is_stack_empty ())) then raise (Failure "Internal Error: stack should be empty after building main function. A function was not popped from the stack after it was built.") 
      else ()

  and add_terminal builder instr =
    (match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder))
  and terminate_block builder retty = add_terminal builder (match retty with
    A.Quack  -> L.build_ret_void
  | A.Float  -> L.build_ret (L.const_float float_t 0.0)
  | A.Int    -> L.build_ret (L.const_int i32_t 0)
  | A.String -> L.build_ret (L.const_int i8_t 0) (* TODO: test and complete this later *)
  | _ -> raise (TODO "unimplemented return types in terminate_block"))
in
(* We only have one top-level function, main. 
   All statements of the sPool program reside within main *)
build_main_function builder statements;

(* Ignore compiler warnings for the 'hello world' submission
   TODO: remove this later
*)
let _ = ltype_of_typ A.Quack in
let _ = build_function builder (false, A.Quack, [], []) in
let _ = voidptr_t in
  
(* Return the final module *)
the_module


(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

let translate (globals, functions) =

  (* Define each function (arguments and return type) so we can 
   * define it's body and call it later *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
	let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	StringMap.add n local m 
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in
     
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in
    
	
    let rec stmt builder = function

      | SReturn e -> let _ = match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder 
                     in builder
      
  List.iter build_function_body functions;
  the_module *)
