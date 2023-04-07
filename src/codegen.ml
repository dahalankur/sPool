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
and list_t     = L.pointer_type (L.struct_type context [| L.pointer_type (L.i8_type context); L.pointer_type (L.named_struct_type context "Node") |])

let ltype_of_typ = function
  A.Int     -> i32_t
| A.Bool    -> i1_t
| A.Float   -> float_t
| A.Quack   -> quack_t
| A.String  -> string_t
| A.List(_) -> list_t (* TODO: Test functions for nested lists.... should still work *)
| _         -> raise (TODO "unimplemented ltype_of_typ for other types")

let is_pointer = function 
  A.List(_) -> true
(* | A.Mutex   -> true *) (* TODO: uncomment when dealing with mutexes *)
| _         -> false

let is_llval_pointer llval = (L.type_of llval = (L.pointer_type (L.pointer_type list_t)))  (* TODO: add for mutex later *)

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

let anon_counter = ref 1

let add_to_scope (s, v, n) builder t =
  if not s then
    let local = L.build_alloca (ltype_of_typ t) n builder in
    let _     = L.build_store v local builder in
    let new_scope = {variables = StringMap.add n local !env.variables; shared = StringMap.add n s !env.shared; parent = !env.parent}
      in env := new_scope 
  else
    if is_pointer t then (* this is for values that are passed by reference (aka raw pointers) *)
      let local = L.build_alloca (L.pointer_type (ltype_of_typ t)) n builder in
      let list  = L.build_load v n builder in
      let _     = L.build_store list local builder in
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

  let string_concat_t      = L.function_type string_t [| string_t; string_t |] in
  let string_concat_func   = L.declare_function "string_concat" string_concat_t the_module in

  let string_substr_t      = L.function_type string_t [| string_t; i32_t; i32_t |] in
  let string_substr_func   = L.declare_function "string_substr" string_substr_t the_module in

  let string_eq_t          = L.function_type i1_t [| string_t; string_t |] in
  let string_eq_func       = L.declare_function "string_eq" string_eq_t the_module in

  let list_insert_t        = L.function_type quack_t [| (L.pointer_type list_t); i32_t; voidptr_t |] in
  let list_insert_func     = L.declare_function "List_insert" list_insert_t the_module in

  let list_len_t           = L.function_type i32_t [| (L.pointer_type list_t) |] in
  let list_len_func        = L.declare_function "List_len" list_len_t the_module in

  let list_int_print_t     = L.function_type quack_t [| (L.pointer_type list_t) |] in
  let list_int_print_func  = L.declare_function "List_int_print" list_int_print_t the_module in

  let list_remove_t       = L.function_type quack_t [| (L.pointer_type list_t); i32_t |] in
  let list_remove_func    = L.declare_function "List_remove" list_remove_t the_module in

  let list_replace_t     = L.function_type quack_t [| (L.pointer_type list_t); i32_t; voidptr_t |] in
  let list_replace_func  = L.declare_function "List_replace" list_replace_t the_module in

  let list_at_t          = L.function_type voidptr_t [| (L.pointer_type list_t); i32_t |] in
  let list_at_func       = L.declare_function "List_at" list_at_t the_module in

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

        let pred_builder      = L.builder_at_end context pred_bb in
        let bool_val          = expr pred_builder predicate in
        
        let _ = push_scope() in
        
        let body_bb           = L.append_block context "while_body" the_function in
            let while_builder = List.fold_left statement (L.builder_at_end context body_bb) body in
        let ()                = add_terminal while_builder (L.build_br pred_bb) in

        let _ = pop_scope() in

        let merge_bb          = L.append_block context "merge" the_function in
        let _                 = L.build_cond_br bool_val body_bb merge_bb pred_builder in L.builder_at_end context merge_bb
    | SAssign (name, ((t, _) as sx)) -> 
        let e' = expr builder sx in 
        if is_pointer t then 
          let heap_ptr = L.build_load e' name builder in
          let new_listlit = L.build_load heap_ptr name builder in (* getting the actual list's heap address in new_listlit *)
          let original_heap_ptr = L.build_load (find_variable !env name) name builder in (* this is the address of the named list variable on the LHS *)
          let _ = L.build_store new_listlit original_heap_ptr builder in builder
        else
          let _  = L.build_store e' (find_variable !env name) builder in builder (* TODO: handle assignment of lambdas here too. should just work I think, because find_variable will return the function definition for that lambda on the rhs. OR MAYBE IT WILL NOT WORK AHHHHHH *)
    | SDefine(s, Arrow(formals, retty), name, (_, e)) ->
      let formal_types = Array.of_list (List.map (fun (t) -> if is_pointer t then L.pointer_type (ltype_of_typ t) else ltype_of_typ t) formals) in
      let ret_llval = if is_pointer retty then (L.pointer_type (ltype_of_typ retty)) else ltype_of_typ retty in
      let ftype = L.function_type ret_llval formal_types in
      let f = L.define_function name ftype the_module in
      let new_scope = {variables = StringMap.add name f !env.variables; shared = StringMap.add name false !env.shared; parent = !env.parent}
        in let _ = env := new_scope in build_named_function name builder e
    | SDefine(s, typ, name, e) -> 
        let e' = expr builder e in
        let _  = add_to_scope (s, e', name) builder typ in builder
    | SReturn (_, SNoexpr) ->  let _ = L.build_ret_void builder in builder
    | SReturn ((t, _) as e) when is_pointer t ->
      let e' = expr builder e in
      let heap_ptr = L.build_load e' "listval" builder in
      let _ = L.build_ret heap_ptr builder in builder
    | SReturn e -> let _ = L.build_ret (expr builder e) builder in builder
  and build_malloc builder llval = 
      let heap = L.build_malloc (L.type_of llval) "heap" builder in
      let _    = L.build_store llval heap builder in
    heap
  and expr builder (t, e) = match e with 
      SNoexpr     -> L.const_int i32_t 0
    | SLiteral i  -> L.const_int i32_t i
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    | SFliteral l -> L.const_float_of_string float_t l
    | SListLit l  ->
      let llvals = List.map (expr builder) l in
      let malloced_ptrs = List.map (build_malloc builder) llvals in (* addresses of malloc'd locations for the llvals on the heap *)
      let list_ptr = L.build_alloca (L.pointer_type list_t) "list_ptr" builder in      (*   Node **l;   *)
      let head = L.build_malloc list_t "head" builder in
      let _ = L.build_store head list_ptr builder in
      let _ = L.build_store (L.const_null list_t) head builder in
      let _ = List.fold_left (fun _ (i, llval) -> 
        (* cast each llval to a void * before inserting it into the list *)
        let void_cast = L.build_bitcast llval voidptr_t "voidptr" builder in
        let listval = L.build_load list_ptr "listval" builder in
        L.build_call list_insert_func [| listval; L.const_int i32_t i; void_cast |] "" builder
      ) list_ptr (List.mapi (fun i llval -> (i, llval)) malloced_ptrs) in list_ptr
    | SVar (s, name)                 -> 
      let llval = (find_variable !env name) in
      if is_llval_pointer llval then llval else L.build_load llval name builder
    | SStringLiteral s               -> L.build_global_stringptr s "strlit" builder
    | SCall ("print", [e])           -> L.build_call printf_func [| str_format_str; (expr builder e) |] "print" builder
    | SCall ("println", [e])         -> L.build_call printf_func [| str_format_str_endline; (expr builder e) |] "println" builder
    | SCall ("int_to_string", [e])   -> L.build_call int_to_string_func [| (expr builder e) |] "int_to_string" builder
    | SCall ("float_to_string", [e]) -> L.build_call float_to_string_func [| (expr builder e) |] "float_to_string" builder
    | SCall ("bool_to_string", [e])  -> L.build_call bool_to_string_func [| (expr builder e) |] "bool_to_string" builder
    | SCall ("int_to_float", [e])    -> L.build_call int_to_float_func [| (expr builder e) |] "int_to_float" builder
    | SCall ("float_to_int", [e])    -> L.build_call float_to_int_func [| (expr builder e) |] "float_to_int" builder
    | SCall ("String_eq", [e1; e2])  -> L.build_call string_eq_func [| (expr builder e1); (expr builder e2) |] "string_eq" builder
    | SCall ("String_len", [e])      -> L.build_call strlen_func [| (expr builder e) |] "strlen" builder
    | SCall ("List_len", [e])        -> 
      let e' = expr builder e in
      let list = L.build_load e' "list" builder in
      L.build_call list_len_func [| list |] "List_len" builder
    | SCall ("List_int_print", [e])  -> 
      let e' = expr builder e in
      let list = L.build_load e' "list" builder in
      L.build_call list_int_print_func [| list |] "" builder
    | SCall ("String_concat", [e1; e2])     -> L.build_call string_concat_func [| (expr builder e1); (expr builder e2) |] "string_concat" builder
    | SCall ("String_substr", [e1; e2; e3]) -> L.build_call string_substr_func [| (expr builder e1); (expr builder e2); (expr builder e3) |] "string_substr" builder
    | SCall ("List_insert", [e1; e2; e3])   -> 
      let e' = expr builder e1 in
      let list = L.build_load e' "list" builder in
      L.build_call list_insert_func [| list; (expr builder e2); (L.build_bitcast (build_malloc builder (expr builder e3)) voidptr_t "voidptr" builder) |] "" builder
    | SCall ("List_remove", [e1; e2])       -> 
      let e' = expr builder e1 in
      let list = L.build_load e' "list" builder in
      L.build_call list_remove_func [| list; (expr builder e2) |] "" builder
    | SCall ("List_replace", [e1; e2; e3])  ->
      let e' = expr builder e1 in 
      let list = L.build_load e' "listval" builder in 
      L.build_call list_replace_func [| list; (expr builder e2); (L.build_bitcast (build_malloc builder (expr builder e3)) voidptr_t "voidptr" builder) |] "" builder
    | SCall ("List_at", [((List(t1), _) as e1); e2]) ->
      let e' = expr builder e1 in 
      let list = L.build_load e' "listval" builder in
      let value = L.build_call list_at_func [| list; (expr builder e2) |] "list_at" builder in
      let cast = 
        if is_pointer t1 then 
          L.build_bitcast value (L.pointer_type (L.pointer_type (L.pointer_type (ltype_of_typ t1)))) "cast" builder
        else L.build_bitcast value (L.pointer_type (ltype_of_typ t1)) "cast" builder in
      L.build_load cast "list_at" builder
    | SCall (f, args) -> 
      (* TODO: deal with store here. If f has already been called with these args, generate instructions to atomically look into the store for the cached answer *)
      let fdef   = find_variable !env f in
      let retty  = L.return_type (L.element_type (L.type_of fdef)) in
      let llargs = (List.map (fun ((t, _) as e) -> if is_pointer t then L.build_load (expr builder e) "ptrval" builder else expr builder e) args) in
      let result = if retty = quack_t then "" else f ^ "_result" in 
      if retty = L.pointer_type list_t then 
        (* special case when lists are being returned from functions *)
        let listptr = L.build_alloca (L.pointer_type list_t) "listptr" builder in
        let head = L.build_malloc list_t "head" builder in
        let _ = L.build_store head listptr builder in
        let _ = L.build_store (L.const_null list_t) head builder in

        let heap_ptr = L.build_call fdef (Array.of_list llargs) result builder in 
        let new_listlit = L.build_load heap_ptr result builder in

        let _ = L.build_store new_listlit head builder in
        listptr
      else
        L.build_call fdef (Array.of_list llargs) result builder
    | SBinop (e1, op, e2) ->
      let (t, _) = e1
      and e1'    = expr builder e1
      and e2'    = expr builder e2 in
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
    | SUnop (op, e) ->
      let (t, _) = e in
      let e' = expr builder e in
      (match op with
          A.Neg when t = A.Float -> L.build_fneg 
        | A.Neg                  -> L.build_neg
        | A.Not                  -> L.build_not 
      ) e' "tmp" builder
    | SThread body -> raise (TODO "unimplemented SThread")
    | (SLambda (store, retty, formals, body)) as e -> 
      let typ = A.Arrow((List.map fst formals), retty) in
      let name = generate_name () in
      let def = SDefine(store, typ, name, (typ, e)) in
      let _ = statement builder def in
      find_variable !env name (* TODO: this is just for returning an llvalue, check later *)
   
      (* TODO: how do we differentiate standalone anonymous lambdas from named functions once we enter here? 
      IDEA: have a function called build_named_function that is called from Define when we have a named function and then have a function called build_anonymous_function that is called from here when we have an anonymous function.     
      this should take care of the problem of having to deal with named functions and anonymous functions in the same place. *)

      (* TODO: maybe we have a separate stringmap for functions that store the "store" info which is then looked at during the scall call...if this is true then we generate instructions to look into the store otherwise we proceed with the call and store the value to our store. *)
      (* TODO: deal with store later here....maybe we don't need to do anything here, only deal with it during SCall? *)    

  (* TODO: remember to deal with scopes, function stack bookkeeping, the_function thingy, etc. Return a builder after building the function *)
  and generate_name () = 
    let name = "#anon_" ^ (string_of_int !anon_counter) in
    let _ = anon_counter := !anon_counter + 1 in
    name
  and add_params_to_scope (s, p, n) builder t = 
  if is_pointer t then 
    let list_ptr = L.build_alloca (L.pointer_type (ltype_of_typ t)) "formal_listptr" builder in
    let _ = L.build_store p list_ptr builder in
    let new_scope = {variables = StringMap.add n list_ptr !env.variables; shared = StringMap.add n s !env.shared; parent = !env.parent}
      in env := new_scope
  else 
    let _ = add_to_scope (s, p, n) builder t in ()
  and build_named_function name builder = function
    SLambda (store, retty, formals, body) ->
      let fdef = find_variable !env name in
      let _ = push_function fdef in
      let _ = push_scope () in

      let fun_builder = L.builder_at_end context (L.entry_block fdef) in

      (* add params to scope *)
      let _ = List.iter2 (fun (t, n) p -> 
        let () = L.set_value_name n p in
        let is_shared = match t with A.List(_) | A.Mutex -> true | _ -> false in
        let _ = add_params_to_scope (is_shared, p, n) fun_builder t in ()) formals (Array.to_list (L.params fdef)) in 

      let final_builder = List.fold_left statement fun_builder body in
      let _ = add_terminal final_builder (L.build_ret (L.const_int i32_t 0)) in

      let _ = pop_scope () in
      let _ = pop_function () in builder
    | _ -> raise (Failure "Internal Error: build_named_function called with non-lambda expression")
  
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
    
    (* End the main function's basic block with a terminal, returning 0 *)
    let _ = add_terminal final_builder (L.build_ret (L.const_int i32_t 0)) in
    
    (* Pop main_function definition from the curr_function stack *)
    let _ = pop_function () in
      if (not (is_stack_empty ())) then raise (Failure "Internal Error: stack should be empty after building main function. A function was not popped from the stack after it was built.") 
      else ()

  and add_terminal builder instr =
    (match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder))
in

(* We only have one top-level function, main. 
   All statements of the sPool program reside within main *)
build_main_function builder statements;

(* Return the final module *)
the_module