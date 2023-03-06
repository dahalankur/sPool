open Ast
open Sast

let check (Program(statements)) =
  let rec check_statement = function
      | Expr(expr) -> raise (TODO "sexpr")
      | Assign(name, expr) -> raise (TODO "sassign")
      | Define(t, name, expr) -> raise (TODO "sdefine")
      | If(expr, statements1, statements2) -> raise (TODO "sif")
      | While(expr, statements) -> raise (TODO "swhile")
      | FunDef(store, t, name, formals, statements) -> raise (TODO "sfunc")
      | Return(expr) -> raise (TODO "sreturn")
  and check_expr = function
      | Literal(l) -> raise (TODO "sliteral")
      | BoolLit(b) -> raise (TODO "sbool")
      | Fliteral(f) -> raise (TODO "sfloat")
      | StringLiteral(s) -> raise (TODO "sstring")
      | ListLit(l) -> raise (TODO "slist")
      | Var(s) -> raise (TODO "svar")
      | Unop(op, e) -> raise (TODO "sunop")
      | Binop(e1, op, e2) -> raise (TODO "sbinop")
      | Lambda(t, formals, statements) -> raise (TODO "slambda")
      | Call(name, actuals) -> raise (TODO "scall")
      | Noexpr -> raise (TODO "snoexpr")
  in
    if statements = [] then raise (TODO "return empty sprogram")
    else raise (TODO "return non-empty sprogram")
