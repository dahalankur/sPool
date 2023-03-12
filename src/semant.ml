open Ast
open Sast

(* exceptions *)
exception SemanticError of string
exception NameNotFound of string
exception TypeError of string

module StringMap = Map.Make(String)

type symbol_table = {
  (* Variables bound in current scope *)
  variables : typ StringMap.t;
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
      | _ -> raise (NameNotFound ("unidentified flying name " ^ name))

      (* TODO: at the end, match indentation levels and style of every function *)
type translation_environment = {
  scope : symbol_table; (* symbol table for vars *)
  (* TODO: may need to add other stuff as we do things *)
}
(* initial env *)
let env : translation_environment ref = ref {
  scope = { variables = StringMap.empty; parent = None };
}
(* TODO: deal with built in functions somewhere here...also need to add additional checks in ASSIGN to ensure someone is not naming their function as a built-in function *)
let check (Program(statements)) =
    let rec eqType = function 
        | (Arrow(ts, t), Arrow(ts2, t2)) -> 
            let sametype = eqType(t, t2) in
            if (List.length ts) != (List.length ts2) then false else
            let zipped_ts = List.combine ts ts2 in
            let sametypes = List.for_all eqType zipped_ts in
              sametype && sametypes
        | (List(t1), List(t2)) -> t1 = Quack || t2 = Quack || eqType(t1, t2)
        | (List(_), _) | (_, List(_)) | (Arrow(_, _), _) | (_, Arrow(_, _)) -> false
        | (t1, t2) -> t1 = t2  (* primitive type equality *)
    in
    let rec eqTypes = function
          | [] -> true
          | [t] -> true
          | t1 :: t2 :: ts -> (eqType(t1, t2)) && (eqTypes (t2 :: ts)) in
    let rec quack_type = function 
        | Quack -> true 
        | List(t) -> quack_type t
        | _ -> false in
    let push_scope () = 
      let new_scope = {variables = StringMap.empty; parent = Some(!env.scope)}
    in env := {scope = new_scope}
  in
    let add_to_scope (t, n) =
      let new_scope = {variables = StringMap.add n t !env.scope.variables; parent = !env.scope.parent}
      in env := {scope = new_scope}
    in
    let pop_scope () = 
      let parent_scope = match !env.scope.parent with 
        Some(parent) -> parent
      | _ -> raise (SemanticError "no parent scope")
    in env := {scope = parent_scope}
    in
    let str_of_scope scope = 
      let str_of_bindings bindings = String.concat ", " (List.map (fun (k, v) -> (string_of_type v) ^ " : " ^ k) (StringMap.bindings bindings)) in
        "{" ^ str_of_bindings scope.variables ^ "}" in
    let defined_in_current_scope s = StringMap.mem s !env.scope.variables in
    let rec check_bool_expr e = 
      let (t, se) = check_expr e in
      let err = "expected " ^ string_of_expr e ^ " to be of type bool, but it is type " ^ string_of_type t ^ " instead" in 
        if t != Bool then raise (TypeError err) else (t, se)
    and check_statement = function
        | Expr(expr)              -> SExpr (check_expr expr)
        | Define(t, name, expr)   ->
          (match check_expr expr with
            | _       when defined_in_current_scope name -> raise (SemanticError ("name " ^ name ^ " is already defined in the current scope and may not be redefined in this scope"))
            | _       when quack_type t -> raise (TypeError ("variables of type quack may not be defined"))
            | (t1, _) when not (eqType(t1, t))  -> raise (TypeError ("expression " ^ string_of_expr expr ^ " of type " ^ string_of_type t1 ^ " may not be assigned to a variable of type " ^ string_of_type t))
            | (t1, sx) as sexpr -> let _ = add_to_scope (t, name) in SDefine(t, name, sexpr))
        | Assign(name, expr)      -> 
          let (t1, sx) as sexpr = check_expr expr in 
            (match find_variable !env.scope name with
              | t when eqType (t1, t) -> SAssign(name, sexpr)
              | t -> raise (TypeError ("expression " ^ string_of_expr expr ^ " of type " ^ string_of_type t1 ^ " may not be assigned to a variable of type " ^ string_of_type t)))
        | While(expr, statements) -> 
          let conditional = check_bool_expr expr in
          let _ = push_scope () in
          let sexpr = SWhile(conditional, List.map check_statement statements) in
          let _ = pop_scope () in sexpr
        | Return(expr)            -> raise (SemanticError "Return statement may not exist outside of a function definition")
        | If(expr, statements1, statements2) -> 
          let conditional = check_bool_expr expr in
          let _ = push_scope () in
          let ss1 = List.map check_statement statements1 in
          let _ = pop_scope () in
          let _ = push_scope () in
          let ss2 = List.map check_statement statements2 in
          let _ = pop_scope () in
            SIf(conditional, ss1, ss2)
        | FunDef(store, t, name, formals, statements) -> 
            check_statement(Assign(name, Lambda(store, t, formals, statements)))
    and check_expr = function
        | Literal(l)          -> (Int, SLiteral l)
        | BoolLit(b)          -> (Bool, SBoolLit(b))
        | Fliteral(f)         -> (Float, SFliteral(f))
        | StringLiteral(s)    -> (String, SStringLiteral(s))
        | Thread(statements)  -> 
          let _ = push_scope () in
          let sts = List.map check_statement statements in
          let _ = pop_scope () in
          (Thread, SThread(sts))
        | ListLit(l) as expr  -> (match l with 
            | [] -> (List(Quack), SListLit([]))
            | xs -> 
              let sxs = List.map check_expr xs in 
              let ts = List.map fst sxs in
                if eqTypes ts then (List(List.hd ts), SListLit(sxs)) 
                else raise (TypeError ("lists must only contain expressions of the same type in expression: " ^ string_of_expr expr)))
        | Var(s)              -> (find_variable !env.scope s, SVar(s))
        | Unop(op, e) as expr -> 
            let (t, se) as sexpr = check_expr e in
              (let ty = match op with
                  Neg when (t = Int || t = Float) -> t
                | Not when t = Bool -> Bool
                | _ -> raise (TypeError ("illegal unary operator " ^ string_of_uop op ^ " on type " ^ string_of_type t ^ " in expression: " ^ string_of_expr expr))
              in (ty, SUnop(op, sexpr)))
        | Binop(e1, op, e2) as expr -> 
            let (t1, se1) as sexpr1 = check_expr e1 in
            let (t2, se2) as sexpr2 = check_expr e2 in
              if not (eqType(t1, t2)) then raise (TypeError ("binary operator " ^ string_of_op op ^ " must get identical types, not " ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ " in expression: " ^ string_of_expr expr)) else
                (let ty = match op with
                  | Add | Sub | Mult | Div when (t1 = Int || t1 = Float) -> t1
                  | Mod when t1 = Int -> Int
                  | And | Or when t1 = Bool -> Bool
                  | Geq | Greater | Leq | Less when (t1 = Int || t1 = Float) -> Bool
                  | Neq | Equal when (t1 = Int || t1 = Bool || t1 = String || t1 = Float) -> Bool
                  | _ -> raise (TypeError ("illegal binary operator " ^ string_of_op op ^ " between types " ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ " in expression: " ^ string_of_expr expr))
                in (ty, SBinop(sexpr1, op, sexpr2)))
        | Lambda(store, t, formals, statements) -> raise (TODO "slambda")
        (* Handle scope for formals here too. Add them to the pushed scope immediately upon entering lambda body, and pop the scope after leaving the lambda body *)
                    (*
              match statements with
                | Return(e) -> let (t', se) = check_expr e
                | _ ->  *)
        | Call(name, actuals) -> raise (TODO "scall")
        | Noexpr -> (Quack, SNoexpr)
      in
        let _ = str_of_scope !env.scope in (* silence compiler warning for functions for debugging *)
        if statements = [] then SProgram([])
        else SProgram(List.map check_statement statements)