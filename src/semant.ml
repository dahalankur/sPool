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

type translation_environment = {
  scope : symbol_table; (* symbol table for vars *)
  (* TODO: may need to add other stuff as we do things *)
}

(* initial env *)
let env : translation_environment ref = ref {
  scope = { variables = StringMap.empty; parent = None };
}

let check (Program(statements)) =
    let push_scope = 
      let new_scope = {variables = StringMap.empty; parent = Some(!env.scope)}
    in env := {scope = new_scope}
  in
    let add_to_scope n t = raise (TODO "add_var_to_scope") in
    let pop_scope = 
      let parent_scope = match !env.scope.parent with 
        Some(parent) -> parent
      | _ -> raise (SemanticError "no parent scope")
    in env := {scope = parent_scope}
  in
    let rec check_bool_expr e = 
      let (t, se) = check_expr e in
      let err = "expected " ^ string_of_expr e ^ " to be of type bool, but it is type " ^ string_of_type t ^ " instead" in 
        if t != Bool then raise (TypeError err) else (t, se)
    and check_statement = function
        | Expr(expr)              -> SExpr (check_expr expr)
        | Define(t, name, expr)   -> raise (TODO "sdefine")
        | Assign(name, expr)      -> raise (TODO "sassign")
        | While(expr, statements) -> 
          let _ = push_scope in
          let sexpr = SWhile(check_bool_expr expr, List.map check_statement statements) in
          let _ = pop_scope in sexpr
        | Return(expr)            -> raise (SemanticError "Return statement may not exist outside of a function definition")
        | If(expr, statements1, statements2) -> 
          let conditional = check_bool_expr expr in
          let _ = push_scope in
          let ss1 = List.map check_statement statements1 in
          let _ = pop_scope in
          let _ = push_scope in
          let ss2 = List.map check_statement statements2 in
          let _ = pop_scope in
            SIf(conditional, ss1, ss2)
        | FunDef(store, t, name, formals, statements) -> raise (TODO "sfunc")
            (* Idea: call Assign with lambda with rhs, deal with store somehow
              match statements with
                | Return(e) -> let (t', se) = check_expr e
                | _ ->  *)
    and check_expr = function
        | Literal(l)          -> (Int, SLiteral l)
        | BoolLit(b)          -> (Bool, SBoolLit(b))
        | Fliteral(f)         -> (Float, SFliteral(f))
        | StringLiteral(s)    -> (String, SStringLiteral(s))
        | Thread(statements)  -> (Thread, SThread(List.map check_statement statements))
        | ListLit(l)          -> raise (TODO "slist")
        | Var(s)              -> raise (TODO "svar")
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
              if t1 <> t2 then raise (TypeError ("binary operator " ^ string_of_op op ^ " must get identical types, not " ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ " in expression: " ^ string_of_expr expr)) else
                (let ty = match op with
                  | Add | Sub | Mult | Div when (t1 = Int || t1 = Float) -> t1
                  | Mod when t1 = Int -> Int
                  | And | Or when t1 = Bool -> Bool
                  | Geq | Greater | Leq | Less when (t1 = Int || t1 = Float) -> Bool
                  | Neq | Equal when (t1 = Int || t1 = Bool || t1 = String || t1 = Float) -> Bool
                  | _ -> raise (TypeError ("illegal binary operator " ^ string_of_op op ^ " between types " ^ string_of_type t1 ^ " and " ^ string_of_type t2 ^ " in expression: " ^ string_of_expr expr))
                in (ty, SBinop(sexpr1, op, sexpr2)))
        | Lambda(t, formals, statements) -> raise (TODO "slambda")
        | Call(name, actuals) -> raise (TODO "scall")
        | Noexpr -> (Quack, SNoexpr)
      in
        if statements = [] then SProgram([])
        else SProgram(List.map check_statement statements)