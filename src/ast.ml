(* Abstract Syntax Tree and functions for printing it *)

type unaryop = Not | Neg
type binop   = And | Or | Add | Sub | Mod | Mult | Div | Equal | Neq | Less 
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
  | Thread of statement list
  | Var of string           
  | Binop of expr * binop * expr 
  | Unop of unaryop * expr      
  | Lambda of bool * typ * bind list * statement list
  | Call of string * expr list
  | Noexpr
and statement = 
    Expr of expr
  | Assign of string * expr
  | Define of bool * typ * string * expr (* first bool indicates whether the variable is shared across threads *)
  | If of expr * statement list * statement list 
  | While of expr * statement list 
  | FunDef of bool * typ * string * bind list * statement list (* first bool indicates whether store is present *)
  | Return of expr

type program = Program of statement list


(* Pretty-printing functions *)

  
let rec string_of_bindings = function
    [] -> ""
  | [(t, x)]  ->  string_of_type t ^ " " ^ x 
  | (t, x)::xs -> string_of_type t ^ " " ^ x ^ ", " ^ string_of_bindings xs
and string_of_expr = function
  Literal(l) -> string_of_int l
| Fliteral(f) -> f
| BoolLit(b) -> string_of_bool b
| StringLiteral(s) -> s
| Thread(s) -> "{...}"
| Var(s) -> s
| Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 
| Unop(o, e) -> string_of_uop o ^ string_of_expr e 
| Lambda(_, t, bs, s) -> "lambda " ^ string_of_type t ^ " (" ^ string_of_bindings bs ^ "): " ^ " ... " ^ ";"
| Call(name, args) -> name ^ "(" ^ (List.fold_left (fun acc e ->  (if acc = "" then string_of_expr e else acc ^ ", " ^ string_of_expr e)) "" args) ^ ")"
| ListLit(es) -> "[" ^ (List.fold_left (fun acc e ->  (if acc = "" then string_of_expr e else acc ^ ", " ^ string_of_expr e)) "" es) ^ "]"
| Noexpr -> ""
(* and string_of_statement n = function 
      Expr(e) -> string_of_expr e
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Define(t, v, e) -> string_of_ty t ^ " " ^ v ^ " = " ^ string_of_expr e
    | Return(e) -> "return " ^ string_of_expr e
    | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ "): " ^ string_of_s_list (n + 1) s1 ^ " else " ^ string_of_s_list (n + 1) s2 ^ ";"
    | While(e, s) -> "while (" ^ string_of_expr e ^ "): " ^ string_of_s_list (n + 1 ) s ^ ";"
    | FunDef(s, t, fname, fs, b) ->  
        "def " ^ (if s then "store " else "") 
        ^ string_of_ty t ^ " " ^ fname ^ " " ^
        "(" ^ str_of_bindings fs ^ "): " ^ string_of_s_list (n + 1) b ^ ")" *)
(* and string_of_s_list n = function
    [] -> ""
  | [s] -> string_of_statement n s
  | s::ss -> string_of_statement n s ^ "" *)
and string_of_type = function
  | Int -> "int"
  | Bool -> "bool"
  | Quack -> "quack"
  | Float -> "float"
  | Mutex -> "mutex"
  | Thread -> "thread"
  | String -> "string"
  | Arrow(ts, t) -> (List.fold_left (fun acc t -> (if acc = "" then acc else acc ^ ", ") ^ string_of_type t) "" ts) ^ " -> " ^ string_of_type t ^ ")"
  | List(t) -> "list<" ^ string_of_type t ^ ">"
  and string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Mod -> "%"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||" 
and string_of_uop = function
    Neg -> "-"
  | Not -> "!"

    
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
    Int -> "INTTY"
  | Bool -> "BOOLTY"
  | Quack -> "QUACKTY"
  | Float -> "FLOATTY"
  | Mutex -> "MUTEXTY"
  | Thread -> "THREADTY"
  | String -> "STRINGTY"
  | Arrow(ts, t) -> "ARROWTY(" ^ (List.fold_left (fun acc t -> (if acc = "" then acc else acc ^ " * ") ^ ast_of_ty t) "" ts) ^ " -> " ^ ast_of_ty t ^ ")"
  | List(t) -> "LISTTY(" ^ ast_of_ty t ^ ")"

let rec ast_of_bindings = function
  [] -> ""
| [(t, x)]  ->  "(" ^ ast_of_ty t ^ ", " ^ x ^ ")"
| (t, x)::xs -> "(" ^ ast_of_ty t ^ ", " ^ x ^ "), " ^ ast_of_bindings xs


let rec ast_of_expr n = function
  Literal(l) -> "INT(" ^ string_of_int l ^ ")"
| Fliteral(f) -> "FLOAT(" ^ f ^ ")"
| BoolLit(b) -> "BOOL(" ^ string_of_bool b ^ ")"
| StringLiteral(s) -> "STRING(" ^ s ^ ")"
| Thread(s) -> "THREAD(" ^ (ast_of_s_list (n + 1) s) ^ ")"
| Var(s) -> "VAR(" ^ s ^ ")"
| Binop(e1, o, e2) -> "BINOP(" ^ ast_of_expr n e1 ^ ", " ^ ast_of_op o ^ ", " ^ ast_of_expr n e2 ^ ")"
| Unop(o, e) -> "UNOP(" ^ ast_of_uop o ^ ", " ^ ast_of_expr n e ^ ")"
| Lambda(_, t, bs, s) -> "LAMBDA(" ^ ast_of_ty t ^ ", " ^ "FORMALS(" ^ ast_of_bindings bs ^ "), " ^ ast_of_s_list (n + 1) s ^ ")"
| Call(name, args) -> "CALL(" ^ name ^ "," ^ " ARGS(" ^ List.fold_left (fun acc ex -> (if acc = "" then acc else acc ^ ", ") ^ ast_of_expr n ex) "" args ^ "))"
| ListLit(es) -> "LIST(" ^ List.fold_left (fun acc ex -> (if acc = "" then acc else acc ^ ", ") ^ ast_of_expr n ex) "" es ^ ")"
| Noexpr -> "NOEXPR"
and 
  ast_of_s_list n s = "[" ^ (List.fold_left (fun acc st -> acc ^ " " ^ ast_of_statement n st) "" s) ^ "]"
and  
  ast_of_statement n statement = 
    let statement_str = 
      match statement with
        Expr(e) -> ast_of_expr n e
        | Assign(v, e) -> "ASSIGN(" ^ v ^ ", " ^ ast_of_expr n e ^ ")"
        | Define(s, t, v, e) -> "DEFINE(" ^ string_of_bool s ^ ", " ^ ast_of_ty t ^ ", " ^ v ^ ", " ^ ast_of_expr n e ^ ")"
        | Return(e) -> "RETURN(" ^ ast_of_expr n e ^ ")"
        | If(e, s1, s2) -> "IF(" ^ ast_of_expr n e ^ ", " ^ ast_of_s_list (n + 1) s1 ^ ", " ^ ast_of_s_list (n + 1) s2 ^ ")"
        | While(e, s) -> "WHILE(" ^ ast_of_expr n e ^ ", " ^ ast_of_s_list (n + 1 ) s ^ ")"
        | FunDef(s, t, fname, fs, b) ->  
            "FUN(" ^ (if s then "STORE, " else "NOSTORE, ") 
            ^ ast_of_ty t ^ ", " ^ fname ^ ", " ^
            "FORMALS(" ^ ast_of_bindings fs ^ "), " ^ ast_of_s_list (n + 1) b ^ ")"
    in 
    "\n" ^ n_tabs n ^ statement_str
and n_tabs n = 
  if n = 0 then "" else "    " ^ (n_tabs (n - 1))
  
let ast_of_program = function
  | Program(statements) -> "PROGRAM {" ^ ast_of_s_list 1 statements  ^ "}"
