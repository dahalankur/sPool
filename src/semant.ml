(* semant.ml
   Walks over the AST generated by the sPool parser and generates a
   semantically-checked AST (SAST) with extra information added to each node 
   of the AST.

   Written by: Team Nautilus (Ankur, Yuma, Max, Etha)
*)

open Ast
open Sast

(* TODO: how to deal with stdlib imports...we need to not forget to auto-import them/bundle them in the compiler later *)

(* exceptions *)
exception SemanticError of string
exception NameNotFound of string
exception TypeError of string

module StringMap = Map.Make(String)

type symbol_table = {
  (* Variables bound in current scope *)
  variables : typ StringMap.t;
  
  (* For each variable bound in current scope, are they shared across thread?
     Store this information in a map from variable name to bool   
  *)
  shared : bool StringMap.t;

  (* Enclosing scope *)
  parent : symbol_table option;
}

let rec find_variable (scope : symbol_table) name =
  try
    (* Try to find binding in nearest block *)
    StringMap.find name scope.variables
  with Not_found -> (* Try looking in outer blocks *)
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _            -> raise (NameNotFound ("unidentified flying name " ^ name))

let rec find_shared (scope : symbol_table) name = 
  try
    (* Try to find binding in nearest block *)
    StringMap.find name scope.shared
  with Not_found -> (* Try looking in outer blocks *)
    match scope.parent with
      Some(parent) -> find_shared parent name
    | _            -> raise (NameNotFound ("unidentified flying name " ^ name))

type translation_environment = {
  scope : symbol_table; (* symbol table for vars *)

  (* TODO: maybe add a list of builtin functions here...? It may help while checking for attempts to define functions that have the same names as the builtins *)
}

(* initial env *)
let env : translation_environment ref = ref {
  scope = { variables = StringMap.empty; shared = StringMap.empty; parent = None };
}

let add_to_scope (s, t, n) =
  let new_scope = {variables = StringMap.add n t !env.scope.variables; shared = StringMap.add n s !env.scope.shared; parent = !env.scope.parent}
  in env := {scope = new_scope}

(* list of built-in function names *)
let builtin_functions = 
  let builtins = [
                              (* List built-ins *)
                  ("List_len", Arrow([List(Alpha)], Int)); ("List_at", Arrow([List(Alpha); Int], Alpha)); 
                  ("List_replace", Arrow([List(Alpha); Int; Alpha], Quack));
                  ("List_insert", Arrow([List(Alpha); Int; Alpha], Quack)); 
                  ("List_remove", Arrow([List(Alpha); Int], Quack)); ("List", Arrow([Int; Alpha], List(Alpha)));
                  
                              (* Printing built-ins *)
                  ("print", Arrow([String], Quack)); ("println", Arrow([String], Quack));

                              (* Type conversion built-ins *)
                  ("int_to_string", Arrow([Int], String)); ("float_to_string", Arrow([Float], String));
                  ("bool_to_string", Arrow([Bool], String)); ("int_to_float", Arrow([Int], Float));
                  ("float_to_int", Arrow([Float], Int));
                  
                              (* String built-ins *)
                  ("String_len", Arrow([String], Int)); ("String_concat", Arrow([String; String], String));
                  ("String_substr", Arrow([String; Int; Int], String)); 

                              (* Mutex built-ins *)
                  ("Mutex", Arrow([Quack], Mutex)); ("Mutex_lock", Arrow([Mutex], Quack));
                  ("Mutex_unlock", Arrow([Mutex], Quack));

                              (* Thread built-ins *)
                  ("Thread_join", Arrow([Thread], Quack));
                  
                  ] in 
  let _ = List.iter (fun (n, t) -> add_to_scope(false, t, n)) builtins in 
  List.map (fun (n, t) -> n) builtins

let builtin_to_alpha = function 
  ("List", _::[x]) -> List(x)
| (_, [])  -> Alpha
| ("List_at", x::_) -> (match x with (List(t)) -> t | _ -> raise (Failure "InternalError: shouldn't happen in builtin_to_alpha"))
| _ -> raise (Failure "InternalError: shouldn't happen in builtin_to_alpha")


let check (Program(statements)) =
  let rec eqType = function 
      (Arrow(ts, t), Arrow(ts2, t2)) -> 
        let sametype = eqType(t, t2) in
        if (List.length ts) != (List.length ts2) then false else
        let zipped_ts = List.combine ts ts2 in
        let sametypes = List.for_all eqType zipped_ts in
          sametype && sametypes
    | (List(t1), List(t2)) -> t1 = Alpha || t2 = Alpha || eqType(t1, t2)
    | (Alpha, _) | (_, Alpha) -> true
    | (List(_), _) | (_, List(_)) | (Arrow(_, _), _) | (_, Arrow(_, _)) -> false
    | (t1, t2) -> t1 = t2  (* primitive type equality *) in
  let rec eqTypes = function
      []             -> true
    | [t]            -> true
    | t1 :: t2 :: ts -> (eqType(t1, t2)) && (eqTypes (t2 :: ts)) in
  let rec quack_type = function 
      Quack   -> true 
    | List(t) -> quack_type t
    | _       -> false in
  let rec alpha_type = function 
      Alpha   -> true 
    | List(t) -> alpha_type t
    | _       -> false in
  let push_scope () = 
    let new_scope = {variables = StringMap.empty; shared = StringMap.empty; parent = Some(!env.scope)}
    in env := {scope = new_scope}
  in
  let pop_scope () = 
    let parent_scope = match !env.scope.parent with 
        Some(parent) -> parent
      | _            -> raise (Failure "Internal Error: no parent scope")
    in env := {scope = parent_scope}
  in
  let str_of_scope scope = 
    let str_of_bindings bindings = String.concat ", " (List.map (fun (k, v) -> (string_of_type v) ^ " : " ^ k) (StringMap.bindings bindings))
    in "{" ^ str_of_bindings scope.variables ^ "}" 
  in
  let defined_in_current_scope s = StringMap.mem s !env.scope.variables in
  let rec check_bool_expr e = 
    let (t, se) = check_expr e in
    let err = "expected " ^ string_of_expr e ^ " to be of type bool, but it is type " ^ string_of_type t ^ " instead" 
    in if t != Bool then raise (TypeError err) else (t, se)
  and construct_arrow_type formals t = 
    let ts = if formals = [] then [Quack] else List.map fst formals in Arrow(ts, t)
  and check_statement = function
      Expr(expr)               -> SExpr (check_expr expr)
    | Define(s, t, name, expr) ->
        (match check_expr expr with
            _       when List.mem name builtin_functions -> raise (SemanticError ("name " ^ name ^ " is a built-in function and may not be redefined"))
          | _       when defined_in_current_scope name -> raise (SemanticError ("name " ^ name ^ " is already defined in the current scope and may not be redefined in this scope"))
          | _       when quack_type t -> raise (TypeError ("variables of type quack may not be defined"))
          | (t1, _) when not (eqType(t1, t))  -> raise (TypeError ("expression " ^ string_of_expr expr ^ " of type " ^ string_of_type t1 ^ " may not be assigned to a variable of type " ^ string_of_type t))
          | (t1, sx) as sexpr -> 
              let _ = (match t1 with
                  Arrow(_, _) | Thread when s -> raise (TypeError ("variables of type " ^ string_of_type t1 ^ " may not be defined as shared variables"))
                | _ -> ()) 
              in
              let is_shared = (match t with List(_) | Mutex -> true
                                          | _               -> s) 
              in
              let _ = add_to_scope (is_shared, t, name)
              in SDefine(is_shared, t, name, sexpr))
    | Assign(name, expr) -> 
        let (t1, sx) as sexpr = check_expr expr in 
            (match find_variable !env.scope name with
                t when eqType (t1, t) -> SAssign(name, sexpr)
              | t -> raise (TypeError ("expression " ^ string_of_expr expr ^ " of type " ^ string_of_type t1 ^ " may not be assigned to a variable of type " ^ string_of_type t)))
    | While(expr, statements) -> 
        let conditional = check_bool_expr expr in
        let _ = push_scope () in
        let sexpr = SWhile(conditional, List.map check_statement statements) in
        let _ = pop_scope () in sexpr
    | Return(expr) -> raise (SemanticError "Return statement may not exist outside of a function definition")
    | If(expr, statements1, statements2) -> 
        let conditional = check_bool_expr expr in
        let _ = push_scope () in
        let ss1 = List.map check_statement statements1 in
        let _ = pop_scope () in
        let _ = push_scope () in
        let ss2 = List.map check_statement statements2 in
        let _ = pop_scope () in SIf(conditional, ss1, ss2)
    | FunDef(store, t, name, formals, statements) ->
        let arrow_ty = construct_arrow_type formals t in
        check_statement(Define(false, arrow_ty, name, Lambda(store, t, formals, statements)))
  and check_expr = function
      Literal(l)          -> (Int, SLiteral l)
    | BoolLit(b)          -> (Bool, SBoolLit(b))
    | Fliteral(f)         -> (Float, SFliteral(f))
    | StringLiteral(s)    -> (String, SStringLiteral(s))
    | Thread(statements)  -> 
        let _ = push_scope () in
        let sts = List.map check_statement statements in
        let _ = pop_scope () in (Thread, SThread(sts))
    | ListLit(l) as expr  -> (match l with 
          [] -> (List(Alpha), SListLit([]))
        | xs -> 
          let sxs = List.map check_expr xs in 
          let ts = List.map fst sxs in
            if eqTypes ts then (List(List.hd ts), SListLit(sxs)) 
            else raise (TypeError ("lists must only contain expressions of the same type in expression: " ^ string_of_expr expr)))
    | Var(s)              -> (find_variable !env.scope s, SVar(find_shared !env.scope s, s))
    | Unop(op, e) as expr -> 
        let (t, se) as sexpr = check_expr e in
          (let ty = match op with
              Neg when (t = Int || t = Float) -> t
            | Not when t = Bool               -> Bool
            | _                               -> raise (TypeError ("illegal unary operator " ^ string_of_uop op ^ " on type " ^ string_of_type t ^ " in expression: " ^ string_of_expr expr))
          in (ty, SUnop(op, sexpr)))
    | Binop(e1, op, e2) as expr -> 
        let (t1, se1) as sexpr1 = check_expr e1 in
        let (t2, se2) as sexpr2 = check_expr e2 in
          if not (eqType(t1, t2)) then raise (TypeError ("binary operator " ^ string_of_op op ^ " must get identical types, not " ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ " in expression: " ^ string_of_expr expr)) else
            (let ty = match op with
              | Add | Sub | Mult | Div     when (t1 = Int || t1 = Float) -> t1
              | Mod                        when t1 = Int                 -> Int
              | And | Or                   when t1 = Bool                -> Bool
              | Geq | Greater | Leq | Less when (t1 = Int || t1 = Float) -> Bool
              | Neq | Equal                when (t1 = Int || t1 = Bool || t1 = String || t1 = Float) -> Bool
              | _ -> raise (TypeError ("illegal binary operator " ^ string_of_op op ^ " between types " ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ " in expression: " ^ string_of_expr expr))
            in (ty, SBinop(sexpr1, op, sexpr2)))
    | Lambda(store, t, formals, statements) -> 
        let count_return s = List.fold_left (fun acc s -> match s with Return(_) -> acc + 1 | _ -> acc) 0 s in
        let num_returns = count_return statements in 
          if not (num_returns = 1) then raise (SemanticError "Function body must have exactly one top-level return statement") else 
        let is_shared t = match t with Mutex | List(_) -> true | _ -> false in
        let _ = push_scope () in
        let _ = List.map (fun (ft, fn) -> add_to_scope (is_shared ft, ft, fn)) formals in (* TODO: add in LRM that only list and mutex formal parameters are marked as shared*)
        (* check whether return statement is the last statement in function body *)
        let rec check_body xs acc = match xs with       
            [Return(e)] -> let (t', se) as sx = check_expr e in
                                if (eqType (t', t)) then SLambda(store, t, formals, (List.rev acc) @ [SReturn(sx)]) 
                                                    else raise (TypeError "Expression in return statement must match type declared in function") 
          | [x] -> raise (SemanticError "No statement(s) can follow a return statement")
          | x :: xs_rest -> check_body xs_rest (check_statement x :: acc)
          | _ -> raise (Failure "InternalError: shouldn't happen in body")
        in let slam = check_body statements [] in 
        let _ = pop_scope () in (construct_arrow_type formals t, slam)
    | Call(name, actuals) -> (* TODO: make error messages more meaningful by including expressions/args in them *)
        let funty = (match find_variable !env.scope name with
            Arrow(_, _) as t -> t
          | _ -> raise (TypeError ("name " ^ name ^ " is not a function and is therefore not callable"))) in 
        let (fty, retty) = match funty with Arrow(f, r) -> (f, r) | _ -> raise (Failure "InternalError: shouldn't happen in call") in
        if List.length actuals != List.length fty then raise (SemanticError ("Function " ^ name ^ " called with the wrong number of arguments")) else
        let checked_actuals = List.map check_expr actuals in
        let arg_typs = List.map fst checked_actuals in
        (* If alphas exist in the functions retty, instantiate it so we get a monomorphic type for the result of the call *)
        let sretty = 
          let pred = alpha_type retty in
            if pred then builtin_to_alpha (name, arg_typs) else retty in
        if eqType (funty, Arrow(arg_typs, retty)) then (sretty, SCall(name, checked_actuals))
        else raise (TypeError ("Function " ^ name ^ " called with the wrong argument types"))
    | Noexpr -> (Quack, SNoexpr)
  in
    let _ = str_of_scope !env.scope in (* silence compiler warning for debugging functions *)
    if statements = [] then SProgram([])
    else SProgram(List.map check_statement statements)