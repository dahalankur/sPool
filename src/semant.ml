open Ast
open Sast

module StringMap = Map.Make(String)
exception SemanticError of string
exception TypeError of string

let check (Program(statements)) =

  let rec check_bool_expr e = 
    let (t', e') = check_expr e in
    let err = "expected " ^ string_of_expr e ^ " to be of type bool, but it is type " ^ string_of_type t' ^ " instead"
    in if t' != Bool then raise (TypeError err) else (t', e')
  and check_statement = function
      | Expr(expr) -> SExpr (check_expr expr)
      | Define(t, name, expr) -> raise (TODO "sdefine")
      | Assign(name, expr) -> raise (TODO "sassign")
      | If(expr, statements1, statements2) -> SIf(check_bool_expr expr, List.map check_statement statements1, List.map check_statement statements2)
      | While(expr, statements) -> SWhile(check_bool_expr expr, List.map check_statement statements)
      | FunDef(store, t, name, formals, statements) -> raise (TODO "sfunc")
          (* Idea: call Assign with lambda with rhs, deal with store somehow
            match statements with
              | Return(e) -> let (t', se) = check_expr e
              | _ ->  *)
      | Return(expr) -> raise (SemanticError "Return statement may not exist outside of a function definition")
  and check_expr = function
      | Literal(l) -> (Int, SLiteral l)
      | BoolLit(b) -> (Bool, SBoolLit(b))
      | Fliteral(f) -> (Float, SFliteral(f))
      | StringLiteral(s) -> (String, SStringLiteral(s))
      | Thread(statements) -> (Thread, SThread(List.map check_statement statements))
      | ListLit(l) -> raise (TODO "slist")
      | Var(s) -> raise (TODO "svar")
      | Unop(op, e) -> 
          let (t, se) as sexpr = check_expr e in
            (let ty = match op with
                Neg when (t = Int || t = Float) -> t
              | Not when t = Bool -> Bool
              | _ -> raise (TypeError ("illegal unary operator " ^ string_of_uop op ^ " on type " ^ string_of_type t))
            in (ty, SUnop(op, sexpr)))
      | Binop(e1, op, e2) -> 
        let (t1, se1) as sexpr1 = check_expr e1 in
        let (t2, se2) as sexpr2 = check_expr e2 in
        if not (t1 = t2) then raise (TypeError ("binary operator " ^ string_of_op op ^ " must get identical types, not " ^ string_of_type t1 ^ " and " ^ string_of_type t2)) else
          (let ty = match op with
            | Add | Sub | Mult | Div when (t1 = Int || t1 = Float) -> t1
            | Mod when t1 = Int -> Int
            | And | Or when t1 = Bool -> Bool
            | Geq | Greater | Leq | Less when (t1 = Int || t1 = Float) -> Bool
            | Neq | Equal when (t1 = Int || t1 = Bool || t1 = String || t1 = Float) -> Bool
            | _ -> raise (TypeError ("illegal binary operator " ^ string_of_op op ^ " between types " ^ string_of_type t1 ^ " and " ^ string_of_type t2))
          in (ty, SBinop(sexpr1, op, sexpr2)))
      | Lambda(t, formals, statements) -> raise (TODO "slambda")
      | Call(name, actuals) -> raise (TODO "scall")
      | Noexpr -> (Quack, SNoexpr)
    in
    if statements = [] then SProgram([])
    else SProgram(List.map check_statement statements)
