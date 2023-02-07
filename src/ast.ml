(* Abstract Syntax Tree and functions for printing it *)


(* type typ = Int | Bool | Float | Void *)

(* type bind = typ * string *)

type unaryop = Not | Neg
type binop   = And | Or | Add | Sub | Mod| Mult | Div | Equal | Neq | Less 
             | Leq | Greater | Geq 

(* type typ = Quack | Int | Bool | Float
type statement = 
  | Assign of typ * string * expr *)

type expr = 
    Literal of int
  | Var of string
  | Binop of expr * binop * expr
  | Unop of unaryop * expr

type statement = 
    Assign of string * expr

type seq =
    Expr of expr * seq
  | Stmnt of statement * seq
  | Eof 

type program = Program of seq
  (* TODO: add more, how to encapsulate other criteria/conditions? *)

  (* 
type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr *)
(* 
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  } *)

(* type program = bind list * func_decl list *)

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

let rec ast_of_expr = function
    Literal(l) -> "LIT(" ^ string_of_int l ^ ")"
  | Var(s) -> "VAR(" ^ s ^ ")"
  | Binop(e1, o, e2) -> "BINOP(" ^ ast_of_expr e1 ^ ", " ^ ast_of_op o ^ ", " ^ ast_of_expr e2 ^ ")"
  | Unop(o, e) -> "UNOP(" ^ ast_of_uop o ^ ", " ^ ast_of_expr e ^ ")"

let string_of_op = function
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

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Var(s) -> s
  (* | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false" *)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  (* | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" *)
  (* | _ -> "" *)
(* 
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)

(* let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void" *)
(* 
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) *)

let ast_of_statement = function
    Assign(v, e) -> "ASSIGN(" ^ v ^ ", " ^ ast_of_expr e ^ ")"

let rec ast_of_seq = function
    Expr(e, Eof)  -> ast_of_expr e 
  | Stmnt(s, Eof) -> ast_of_statement s
  | Expr(e, sequence)  -> ast_of_expr e ^ ", " ^ ast_of_seq sequence
  | Stmnt(s, sequence) -> ast_of_statement s ^ ", " ^ ast_of_seq sequence
  | Eof -> ""

let ast_of_program = function
  | Program(sequence) -> "PROGRAM[" ^ ast_of_seq sequence ^ "]"