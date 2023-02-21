(* Abstract Syntax Tree and functions for printing it *)

type unaryop = Not | Neg
type binop   = And | Or | Add | Sub | Mod| Mult | Div | Equal | Neq | Less 
             | Leq | Greater | Geq 

type typ = Quack | Int | Bool | Float | Mutex | Thread | String 
          | Arrow of typ list * typ
          | List of typ

type bind = typ * string

type expr = 
    Literal of int
  | BoolLit of bool
  | ListLit of expr list
  | Fliteral of string
  | StringLiteral of string
  | Var of string
  | Binop of expr * binop * expr
  | Unop of unaryop * expr
  | Lambda of typ * bind list * statement list
  | Call of string * expr list 
  | Noexpr
and statement = 
    Expr of expr
  | Assign of string * expr
  | Define of typ * string * expr
  | If of expr * statement list * statement list
  | While of expr * statement list
  | FunDef of bool * typ * string * bind list * statement list (* first bool indicates whether store is present *)
  | Return of expr

type program = Program of statement list


(* Pretty-printing functions *)

let ast_of_op = function
    Add -> "PLUS"
  | Sub -> "MINUS"
  | Mult -> "TIMES"
  | Mod -> "MOD"
  | Div -> "DIV"
  | Equal -> "EQUALS"
  | Neq -> "NOTEQUALS"
  | Less -> "LESS"
  | Leq -> "LEQ"
  | Greater -> "GREATER"
  | Geq -> "GEQ"
  | And -> "AND"
  | Or -> "OR" 
  
let ast_of_uop = function
    Neg -> "NEG"
  | Not -> "NOT"

let rec ast_of_ty = function
    Int -> "INT"
  | Bool -> "BOOL"
  | Quack -> "QUACK"
  | Float -> "FLOAT"
  | Mutex -> "MUTEX"
  | Thread -> "THREAD"
  | String -> "STRING"
  | Arrow(ts, t) -> "ARROW(" ^ (List.fold_left (fun acc t -> acc ^ " " ^ ast_of_ty t) "" ts) ^ ", " ^ ast_of_ty t ^ ")"
  | List(t) -> "LISTTY(" ^ ast_of_ty t ^ ")"

let rec str_of_bindings = function
    [] -> ""
  | (t, x)::xs -> "(" ^ ast_of_ty t ^ ", " ^ x ^ ") " ^ str_of_bindings xs
  
let rec ast_of_expr n = function
  Literal(l) -> "INT(" ^ string_of_int l ^ ")"
| Fliteral(f) -> "FLOAT(" ^ f ^ ")"
| BoolLit(b) -> "BOOL(" ^ string_of_bool b ^ ")"
| StringLiteral(s) -> "STRING(" ^ s ^ ")"
| Var(s) -> "VAR(" ^ s ^ ")"
| Binop(e1, o, e2) -> "BINOP(" ^ ast_of_expr n e1 ^ ", " ^ ast_of_op o ^ ", " ^ ast_of_expr n e2 ^ ")"
| Unop(o, e) -> "UNOP(" ^ ast_of_uop o ^ ", " ^ ast_of_expr n e ^ ")"
| Lambda(t, bs, s) -> "LAMBDA(" ^ ast_of_ty t ^ ", " ^ "FORMALS(" ^ str_of_bindings bs ^ "), " ^ ast_of_s_list (n + 1) s ^ ")"
| Call(name, args) -> "CALL(" ^ name ^ ", " ^ " ARGS(" ^ List.fold_left (fun acc ex -> acc ^ " " ^ ast_of_expr n ex) "" args ^ "))"
| ListLit(es) -> "LIST(" ^ List.fold_left (fun acc ex -> acc ^ " " ^ ast_of_expr n ex) "" es ^ ")"
| Noexpr -> "NOEXPR"
and 
  ast_of_s_list n s = "[" ^ (List.fold_left (fun acc st -> ast_of_statement n st ^ " " ^ acc) "" s) ^ "]"
and  
  ast_of_statement n statement = 
    let statement_str = 
      match statement with
        Expr(e) -> ast_of_expr n e
        | Assign(v, e) -> "ASSIGN(" ^ v ^ ", " ^ ast_of_expr n e ^ ")"
        | Define(t, v, e) -> "DEFINE(" ^ ast_of_ty t ^ ", " ^ v ^ ", " ^ ast_of_expr n e ^ ")"
        | Return(e) -> "RETURN(" ^ ast_of_expr n e ^ ")"
        | If(e, s1, s2) -> "IF(" ^ ast_of_expr n e ^ ", " ^ ast_of_s_list (n + 1) s1 ^ ", " ^ ast_of_s_list (n + 1) s2 ^ ")"
        | While(e, s) -> "WHILE(" ^ ast_of_expr n e ^ ", " ^ ast_of_s_list (n + 1 ) s ^ ")"
        | FunDef(s, t, fname, f, b) ->  
            "FUN(" ^ (if s then " STORE, " else " NOSTORE,") 
            ^ ast_of_ty t ^ ", " ^ fname ^ ", " ^
            "FORMALS(" ^ List.fold_left (fun acc (ty, x) -> acc ^ " " ^ "(" ^ ast_of_ty ty ^ ", " ^ x ^ ")") "" f ^ "), " ^ ast_of_s_list (n + 1) b ^ ")"
    in 
    "\n" ^ n_tabs n ^ statement_str
and n_tabs n = 
  if n = 0 then "" else "    " ^ (n_tabs (n - 1))
  
let ast_of_program = function
  | Program(statements) -> "PROGRAM {" ^ ast_of_s_list 1 statements  ^ "}"