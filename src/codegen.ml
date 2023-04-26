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
let i32_t      = L.i32_type    context
and i8_t       = L.i8_type     context
and i1_t       = L.i1_type     context
and float_t    = L.double_type context
and quack_t    = L.void_type   context 
and string_t   = L.pointer_type (L.i8_type context) 
and voidptr_t  = L.pointer_type (L.i8_type context)
and nodeptr_t  = L.pointer_type (L.named_struct_type context "Node") 

let list_t     = L.pointer_type (L.struct_type context [| voidptr_t; nodeptr_t |])
and pthread_t  = L.pointer_type (L.named_struct_type context "pthread_t")
(* and mutex_t    = L.pointer_type (L.named_struct_type context "pthread_mutex_t") *)

let thread_t   = pthread_t 

let is_pointer = function 
  A.List(_) -> true
(* | A.Mutex   -> true *)
| _         -> false

let rec ltype_of_typ = function
  A.Int     -> i32_t
| A.Bool    -> i1_t
| A.Float   -> float_t
| A.Quack   -> quack_t
| A.String  -> string_t
| A.List(_) -> list_t
| A.Thread  -> thread_t
(* | A.Mutex   -> mutex_t *)
| A.Arrow(ts, t) -> ftype_from_t (A.Arrow(ts, t)) (* TODO: while dealing with return values from functions or params to functions, cast function types to fptrs instead to make llvm (and our svar, slambda, etc.) happy *)
| _         -> raise (TODO "unimplemented ltype_of_typ for other types")
and ftype_from_t = function
  A.Arrow(formals, retty) -> 
    let formal_types = Array.of_list (List.map (fun (t) -> if ((is_pointer t) || (is_function t)) then L.pointer_type (ltype_of_typ t) else ltype_of_typ t) formals) in
    let formal_types = Array.of_list (List.filter (fun (t) -> t <> quack_t) (Array.to_list formal_types)) in
    let ret_llval = 
      if ((is_pointer retty) || (is_function retty)) then (L.pointer_type (ltype_of_typ retty)) else ltype_of_typ retty in L.function_type ret_llval formal_types
| _ -> raise (Failure "Internal Error: ftype_from_t called on non-arrow type")
and is_function = function A.Arrow(_, _) -> true | _ -> false

let is_llval_pointer llval = (L.type_of llval = (L.pointer_type (L.pointer_type list_t)))  (* TODO: add for mutex later *)

type symbol_table = {
  variables : L.llvalue StringMap.t;
  functionpointers : L.llvalue StringMap.t;
  shared : bool StringMap.t;
  stored : bool StringMap.t;
  parent : symbol_table option;
}

let rec find_variable (scope : symbol_table) name =
  try
    StringMap.find name scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _            -> raise (Failure ("Internal Error: should have been caught in semantic analysis (find_variable)"))

let rec find_shared (scope : symbol_table) name = 
  try
    StringMap.find name scope.shared
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_shared parent name
    | _            -> raise (Failure ("Internal Error: should have been caught in semantic analysis (find_shared)"))

let rec find_stored (scope : symbol_table) name = 
    try
      StringMap.find name scope.stored
    with Not_found ->
      match scope.parent with
        Some(parent) -> find_stored parent name
      | _            -> raise (Failure ("Internal Error: should have been caught in semantic analysis (find_stored)"))

(* initial env *)
let env : symbol_table ref = ref { variables = StringMap.empty; shared = StringMap.empty; stored = StringMap.empty; parent = None ; functionpointers = StringMap.empty }



let seen_functions = ref StringMap.empty

let anon_counter = ref 1

let add_to_scope (s, v, n) builder t =
  if not s then
    let local = L.build_alloca (ltype_of_typ t) n builder in
    let _     = L.build_store v local builder in
    let new_scope = {variables = StringMap.add n local !env.variables; shared = StringMap.add n s !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = !env.functionpointers}
      in env := new_scope 
  else
    if is_pointer t then (* this is for values that are passed by reference (aka raw pointers) *)
      let local = L.build_alloca (L.pointer_type (ltype_of_typ t)) n builder in
      let list  = L.build_load v n builder in
      let _     = L.build_store list local builder in
      let new_scope = {variables = StringMap.add n local !env.variables; shared = StringMap.add n s !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = !env.functionpointers}
        in env := new_scope
    else
      let local = L.build_alloca (L.pointer_type (ltype_of_typ t)) n builder in
      let heap  = L.build_malloc (ltype_of_typ t) n builder in
      let _     = L.build_store heap local builder in
      let _     = L.build_store v heap builder in
      let new_scope = {variables = StringMap.add n heap !env.variables; shared = StringMap.add n s !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = !env.functionpointers}
        in env := new_scope

let depth = ref 0

let push_scope () = 
  let new_scope = {variables = StringMap.empty; shared = StringMap.empty; stored = StringMap.empty; parent = Some(!env); functionpointers = StringMap.empty}
  in let _ = depth := !depth + 1 in env := new_scope

let pop_scope () = 
  let parent_scope = match !env.parent with 
      Some(parent) -> parent
    | _            -> raise (Failure "Internal Error: should not happen in pop_scope; should have been caught in semantic analysis")
  in let _ = depth := !depth - 1 in env := parent_scope

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

  (* if we are a nested function, how many hops are we away from our parent 
     function in terms of jumps in the symbol table? 
  *)
  (* let hops_to_parent_function = ref 0 in  *)

  (* if top is stack is something other than "main", we are generating a nested function *)
  let is_nested_fun () = not (is_stack_empty () || (String.equal (L.value_name (the_function ())) "main")) in

  (* ----------- end of stack-related bookkeeping functions ----------- *)

  let list_of_llvals_n_hops_from_scope (scope : symbol_table) hops builder = 
    let rec helper acc curr_scope remaining_hops = 
      if (remaining_hops < 0) then raise (Failure "Internal Compiler Error: should not happen in list_of_llvals_n_hops_from_scope; scope is not correct") else
      (match curr_scope.parent with 
        None -> acc @ (StringMap.bindings curr_scope.variables)
      | Some(p) -> if (remaining_hops = 0) then (acc @ (StringMap.bindings curr_scope.variables)) else (helper (acc @ (StringMap.bindings curr_scope.variables)) p (remaining_hops - 1)))
    in (helper [] scope hops, builder) in
  
  (* dumps the symbol table to a list of (name, llvalue) pairs *)
  let list_of_llvals (scope : symbol_table) builder = 
    let rec helper acc curr_scope = 
      (match curr_scope.parent with 
        None -> acc @ (StringMap.bindings curr_scope.variables)
      | Some(p) -> helper (acc @ (StringMap.bindings curr_scope.variables)) p)
    in 
    (* prevent nested functions from capturing old, stale scope. Nested functions need to only look at their current parent 
       since their parent will have captured everything and added to their scope by this point anyway *)
    let (list_llvals, builder) = if (is_nested_fun ()) then (list_of_llvals_n_hops_from_scope !env (!depth - 1) builder) else (helper [] scope, builder) in 
    let seen_names = ref StringMap.empty in
    let result = List.rev (List.fold_left (fun acc (name, llval) -> 
      if StringMap.mem name !seen_functions then acc else 
      if StringMap.mem name !seen_names then acc else
      let _ = seen_names := StringMap.add name true !seen_names in
      if (is_llval_pointer llval) then (name, L.build_load llval name builder) :: acc else
      if (find_shared !env name) then (name, llval) :: acc
      else (name, L.build_load llval name builder) :: acc)
      [] list_llvals) in (result, builder) in 
  
    let list_of_fptrs (scope : symbol_table) builder =
      let rec helper acc curr_scope = 
        (match curr_scope.parent with 
          None -> acc @ (StringMap.bindings curr_scope.functionpointers)
        | Some(p) -> helper (acc @ (StringMap.bindings curr_scope.functionpointers)) p)
      in 
      let list_fptrs = helper [] scope in
      let result = List.rev (List.fold_left (fun acc bs -> bs :: acc) [] list_fptrs)
      in (result, builder) in

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
  let int_to_float_func    = L.declare_function "int_to_float" int_to_float_t the_module in

  let float_to_int_t       = L.function_type i32_t [| float_t |] in
  let float_to_int_func    = L.declare_function "float_to_int" float_to_int_t the_module in

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


  (* ----- start of thread related function declarations ----- *)
  let pthread_create_t          = L.function_type i32_t [| (L.pointer_type pthread_t); voidptr_t; (L.pointer_type (L.function_type voidptr_t [| voidptr_t |])); voidptr_t |] in 
  let pthread_join_t            = L.function_type i32_t [| pthread_t; (L.pointer_type voidptr_t) |] in
  (* let pthread_mutex_init_t      = L.function_type (L.pointer_type mutex_t) [| |] in
  let pthread_mutex_lock_t      = L.function_type i32_t [| mutex_t |] in
  let pthread_mutex_unlock_t    = L.function_type i32_t [| mutex_t |] in *)

  let pthread_create_func       = L.declare_function "pthread_create" pthread_create_t the_module in 
  let pthread_join_func         = L.declare_function "pthread_join" pthread_join_t the_module in
  (* let pthread_mutex_init_func   = L.declare_function "Mutex_init" pthread_mutex_init_t the_module in
  let pthread_mutex_lock_func   = L.declare_function "pthread_mutex_lock" pthread_mutex_lock_t the_module in
  let pthread_mutex_unlock_func = L.declare_function "pthread_mutex_unlock" pthread_mutex_unlock_t the_module in *)
  (* ----- end of thread related function declarations ----- *)


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
        (match t with 
          A.Arrow(_, _) ->
            let fptr = e' in 
            let new_scope = {variables = StringMap.add name fptr !env.variables; shared = StringMap.add name false !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = StringMap.add name fptr !env.functionpointers}
            in let _ = env := new_scope in builder
          | _ ->
            if is_pointer t then 
              let lhs = find_variable !env name in
              let _ = L.build_store (L.build_load e' "rhs" builder) lhs builder in builder
            else
              let _  = L.build_store e' (find_variable !env name) builder in builder)
    | SDefine(_, A.Arrow(formals, retty), name, (t, e)) ->
      (match e with
        (* there is no function to build here; assign to pre-existing function ptr *)
        SVar(_, rhs) -> let fptr = find_variable !env rhs in  
          let new_scope = {variables = StringMap.add name fptr !env.variables; shared = StringMap.add name false !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = StringMap.add name fptr !env.functionpointers}
            in let _ = env := new_scope in builder
        | SCall(_, _) -> let fptr = expr builder (t , e) in 
                            let new_scope = {variables = StringMap.add name fptr !env.variables; shared = StringMap.add name false !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = StringMap.add name fptr !env.functionpointers}
                            in let _ = env := new_scope in builder
      | SLambda (store, _, _, _) ->
        let ftype = ftype_from_t (A.Arrow(formals, retty)) in
        let f = L.define_function name ftype the_module in
        let fptr = L.build_bitcast f (L.pointer_type ftype) (name ^ "_ptr") builder in
        let new_scope = {variables = StringMap.add name fptr !env.variables; shared = StringMap.add name false !env.shared; stored = StringMap.add name store !env.stored; parent = !env.parent; functionpointers = StringMap.add name fptr !env.functionpointers}
          in let _ = env := new_scope in build_named_function name builder e)
    | SDefine(s, typ, name, e) -> let _  = add_to_scope (s, (expr builder e), name) builder typ in builder
    | SReturn (_, SNoexpr) ->  let _ = L.build_ret_void builder in builder
    | SReturn ((t, _) as e) when is_pointer t ->
      let listval = L.build_load (expr builder e) "listval" builder in
      let _ = L.build_ret listval builder in builder
    | SReturn e -> let _ = L.build_ret (expr builder e) builder in builder
  and build_malloc builder llval = 
      let heap = L.build_malloc (L.type_of llval) "heap" builder in
      let _    = L.build_store llval heap builder in heap
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
    | SVar (_, name)                 -> let llval = find_variable !env name in 
                                        if ((is_llval_pointer llval) || (is_function t)) then llval (* just a pointer to a val or a function, hence no need to load *)
                                        else L.build_load llval name builder
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
    | SCall ("List_at", [((A.List(t1), _) as e1); e2]) ->
      let e' = expr builder e1 in 
      let list = L.build_load e' "listval" builder in
      let value = L.build_call list_at_func [| list; (expr builder e2) |] "list_at" builder in
      let cast = 
        if is_pointer t1 then 
          L.build_bitcast value (L.pointer_type (L.pointer_type (L.pointer_type (ltype_of_typ t1)))) "cast" builder (* TODO: how do we deal with mutexes here? WHY ARE MUTEXES PASSED BY REFERENCE TO BEGIN WITH!? *)
        else L.build_bitcast value (L.pointer_type (ltype_of_typ t1)) "cast" builder in
      L.build_load cast "list_at" builder
    | SCall ("Thread_join", [e]) -> L.build_call pthread_join_func [| expr builder e; L.const_null (L.pointer_type voidptr_t) |] "" builder
    (* | SCall ("Mutex", []) -> raise (TODO "Mutex")
    | SCall ("Mutex_lock", [e]) -> let _ = L.build_call pthread_mutex_lock_func [| L.build_load (expr builder e) "mutex" builder |] "" builder in raise (TODO "Mutex_lock")
    | SCall ("Mutex_unlock", [e]) -> let _ = L.build_call pthread_mutex_unlock_func [| L.build_load (expr builder e) "mutex" builder |] "" builder in raise (TODO "Mutex_unlock") *)
    | SCall (f, args) -> 
       let is_storefunc = find_stored !env f in 
      (* if is_storefunc then 
          let predicate = ll_lookup 
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
          
            
    

      else  *)
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
        if is_storefunc then 
        let resultval = L.build_call fdef (Array.of_list llargs) result builder in
        let curr_index = L.build_struct_gep global_store_struct 0 "" builder in
        let full_indicator = L.build_struct_gep global_store_struct 1 "" builder in
        let elem_array = L.build_struct_gep global_store_struct 2 "" builder in
        let current_elem = L.build_struct_gep elem_array curr_index "" builder in
        let new_store_elem_struct = 
        let _ = L.build_store (*new struct*) current_elem builder in
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
      else (match op with
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
    | SThread body -> 
      let funtype  = A.Arrow([], A.Quack) in 
      let fdef     = expr builder (funtype, SLambda(false, A.Quack, [], body)) in 
      let fcast    = L.build_bitcast fdef (L.pointer_type (L.function_type voidptr_t [|voidptr_t|])) "fptr_cast" builder in (* complying with pthread_create's signature *)
      let pthread_t_ref = L.build_alloca pthread_t "pthread_t" builder in
      let _ = L.build_call pthread_create_func [|pthread_t_ref; L.const_null voidptr_t; fcast; L.const_null voidptr_t|] "" builder in
      L.build_load pthread_t_ref "pthread_t" builder
    | (SLambda (store, retty, formals, _)) as e -> 
      let typ = A.Arrow((List.map fst formals), retty) in
      let name = generate_name () in
      let _ = statement builder (SDefine(store, typ, name, (typ, e))) in
      find_variable !env name
  and generate_name () = 
    let name = "#anon_" ^ (string_of_int !anon_counter) in
    let _ = anon_counter := !anon_counter + 1 in
    name
  and add_params_to_scope (s, p, n) builder t = 
  if is_pointer t then 
    let list_ptr = L.build_alloca (L.pointer_type (ltype_of_typ t)) "formal_listptr" builder in
    let _ = L.build_store p list_ptr builder in
    let new_scope = {variables = StringMap.add n list_ptr !env.variables; shared = StringMap.add n s !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = !env.functionpointers}
      in env := new_scope
  else if is_function t then
    let new_scope = {variables = StringMap.add n p !env.variables; shared = StringMap.add n s !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = StringMap.add n p !env.functionpointers}
      in let _ = env := new_scope 
      in seen_functions := StringMap.add n true !seen_functions
  else
    let _ = add_to_scope (s, p, n) builder t in ()
  
  (* creates a struct with field {llval}, fills it up with the provided
     llval argument and returns the created struct
  *)
  and struct_of_llval fname builder binding =
    let name_str = "(" ^ fname ^ "):" ^ (fst binding) ^ ":" in
    let str_type = L.named_struct_type context name_str in
    let v = snd binding in
    let _ = L.struct_set_body str_type [| L.type_of v |] false in
    let struct_alloc = L.build_alloca str_type (fname ^ "__struct__" ^ (fst binding)) builder in
    let vptr = L.build_struct_gep struct_alloc 0 "v" builder in
    let _ = L.build_store v vptr builder in
    L.build_load struct_alloc name_str builder
  
  (* dumps the current scope into a list of structs, each struct containing an llval.
     returns the list of structs and the updated builder   
  *)
  and dump_scope fname builder = 
    (* add this function to the seen list. This prevents functions from being 
    captured by themselves and other functions. *)
    let _ = seen_functions := StringMap.add fname true !seen_functions in
    let (llval_bindings, builder) = list_of_llvals !env builder in 
    let seen_names = ref StringMap.empty in 
    let llval_bindings = List.fold_left (fun acc (n, v) -> 
        (* skip ourself and any other functions *)
        if String.equal n fname then acc else if StringMap.mem n !seen_functions then acc else
        let has_seen = StringMap.mem n !seen_names in
        let answer = if has_seen then acc else let _ = seen_names := StringMap.add n true !seen_names in (n, v) :: acc
        in  answer) [] llval_bindings in (List.map (struct_of_llval fname builder) llval_bindings, builder)
  and build_named_function name builder = function
    SLambda (store, retty, formals, body) ->
      let _ = if store then 
        let formal_lltys = List.map (fun (t, _) -> ltype_of_typ t) formals in
        let ret_llty     = ltype_of_typ retty in 
        let param_struct = L.named_struct_type context (name ^ "_param_struct#") in
        let _ = L.struct_set_body param_struct (Array.of_list (formal_lltys)) false in 
        let store_elem_struct = L.named_struct_type context (name ^ "_store_elem_struct#") in
        let _ = L.struct_set_body store_elem_struct [| param_struct ; ret_llty |] false in 
        let store_elem_arrayty = (L.array_type store_elem_struct 32) in
        let store_struct = L.named_struct_type context (name ^ "_store_struct#") in
        (* a store struct contains { latest_index, full_marker, element array } *)
        let _ = L.struct_set_body store_struct [| L.i32_type context; L.i1_type context; store_elem_arrayty |] false in 
        let global_store_struct = L.define_global ("global_" ^ name ^ "_store#") (L.const_null store_struct) the_module in
        ()(*build array*)
      else () in
      let closure_struct = L.named_struct_type context (name ^ "_closure_struct#") in
      let (dumped_scope, builder) = dump_scope name builder in

      let (fptrs, builder) = list_of_fptrs !env builder in
      let fptrs = List.filter (fun (n, _) -> not (String.equal n name)) fptrs in
      let struct_of_fptrs = List.map (struct_of_llval name builder) fptrs in 
      let all_structs = dumped_scope @ struct_of_fptrs in
      let non_fptr_bound = List.length dumped_scope in

      let dumped_scope = all_structs in

      let dumped_scope_tys = List.map L.type_of dumped_scope in
      let _ = L.struct_set_body closure_struct (Array.of_list (dumped_scope_tys)) false in 
      let global_closure_struct = L.define_global ("global_" ^ name ^ "_closure#") (L.const_null closure_struct) the_module in
     
      (* fill up global closure struct with individual structs of captured variables *)
      let _ = List.fold_left (fun index dumped_llval -> 
        let ith_struct = L.build_struct_gep global_closure_struct index "" builder in
        let _ = L.build_store dumped_llval ith_struct builder in
      index + 1) 0 dumped_scope in ();

      (* build function body *)
      let fdef = find_variable !env name in
      let _ = push_function fdef in
      let _ = push_scope () in

      let fun_builder = L.builder_at_end context (L.entry_block fdef) in

      (* unpacking the variables in the closure *)

      let _ = List.iteri (fun index _ -> 
        let var_struct_ptr = L.build_struct_gep global_closure_struct index "" fun_builder in
        let struct_var = L.build_load var_struct_ptr "individual_data_struct" fun_builder in
        let struct_typ = L.type_of struct_var in 

        let struct_local = L.build_alloca struct_typ "" fun_builder in
        let _ = L.build_store struct_var struct_local fun_builder in

        let struct_name = match (L.struct_name struct_typ) with Some(s) -> s | None -> "" in 
        let var_name = List.nth (String.split_on_char ':' struct_name) 1 in 

        let is_shared = find_shared !env var_name in 
        
        let llval_ptr = L.build_struct_gep struct_local 0 "" fun_builder in
        let llval = L.build_load llval_ptr var_name fun_builder in
        let is_list = (L.type_of llval = (L.pointer_type list_t)) in 

        let shared_and_not_list = is_shared && (not is_list) in

        if (index < non_fptr_bound) then
          let llval_local = L.build_alloca (L.type_of llval) (var_name ^ "_ptr") fun_builder in
          let _ = L.build_store llval llval_local fun_builder in

          let address = if (shared_and_not_list) then (L.build_load llval_local "" fun_builder) else llval_local in

          let _ = let new_scope = {variables = StringMap.add var_name address !env.variables; shared = StringMap.add var_name is_shared !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = !env.functionpointers} in
          env := new_scope in ()
        else
           (* we are unpacking captured fptrs here now *)
          let _ = let new_scope = {variables = StringMap.add var_name llval !env.variables; shared = StringMap.add var_name is_shared !env.shared; stored = !env.stored; parent = !env.parent; functionpointers = !env.functionpointers} in
          env := new_scope in ()) dumped_scope in
      
      if List.length formals > 0 then
        (* add params to scope *)
        let _ = List.iter2 (fun (t, n) p -> 
          let () = L.set_value_name n p in
          let is_shared = match t with A.List(_) | A.Mutex -> true | _ -> false in
          let _ = add_params_to_scope (is_shared, p, n) fun_builder t in ()) formals (Array.to_list (L.params fdef)) in ()
      else ();
    
      (* build body *)
      let final_builder = List.fold_left statement fun_builder body in
      let instr = if retty = A.Quack then L.build_ret_void else L.build_ret (L.const_int i32_t 0) in
      let _ = add_terminal final_builder instr in
      
      let _ = pop_scope () in
      let _ = pop_function () in builder
    | _ -> raise (Failure "Internal Error: non-lambda expression passed to build_named_function")
  
  and build_main_function builder statements =
    (* Note to self: at this point, final_builder is pointing to the END of the main function. 
       The call to statement generates instructions for all statments in this main function, 
       which subsequently keeps on updating the instruction builder. Therefore, after the last 
       instruction in main's body is generated, the builder points to that instruction and 
       this is stored in final_builder. This is different that the `builder` in the argument since that 
       builder is still pointing to the beginning of the main function! *)
    
    (* Push main_function definition to the curr_function stack first *)
    let _ = push_function main_function in

    let final_builder = List.fold_left statement builder statements in
    
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
the_module;