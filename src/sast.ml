open Ast

exception TODO of string (* remove when done *)

type sexpr = typ * sx  (* bool indicates if this typ is shared across all threads *)
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SListLit of sexpr list
  | SFliteral of string
  | SStringLiteral of string
  | SThread of sstatement list
  | SVar of string           
  | SBinop of sexpr * binop * sexpr 
  | SUnop of unaryop * sexpr      
  | SLambda of bool * typ * bind list * sstatement list
  | SCall of string * sexpr list
  | SNoexpr
and sstatement = 
    SExpr of sexpr
  | SAssign of string * sexpr
  | SDefine of bool * typ * string * sexpr
  | SIf of sexpr * sstatement list * sstatement list 
  | SWhile of sexpr * sstatement list 
  | SReturn of sexpr

type sprogram = SProgram of sstatement list


(* Pretty-printing functions *)

let rec sast_of_sexpr n (t, e) = 
  "(" ^ ast_of_ty t ^ " : " ^
  (match e with
    SLiteral(l) -> "SINT(" ^ string_of_int l ^ ")"
  | SFliteral(f) -> "SFLOAT(" ^ f ^ ")"
  | SBoolLit(b) -> "SBOOL(" ^ string_of_bool b ^ ")"
  | SStringLiteral(s) -> "SSTRING(" ^ s ^ ")"
  | SThread(s) -> "STHREAD(" ^ (sast_of_s_list (n + 1) s) ^ ")"
  | SVar(s) -> "SVAR(" ^ ast_of_ty t ^ ", " ^ s ^ ")"
  | SBinop(e1, o, e2) -> "SBINOP(" ^ sast_of_sexpr n e1 ^ ", " ^ ast_of_op o ^ ", " ^ sast_of_sexpr n e2 ^ ")"
  | SUnop(o, e) -> "SUNOP(" ^ ast_of_uop o ^ ", " ^ sast_of_sexpr n e ^ ")"
  | SLambda(store, t1, bs, s) -> "SLAMBDA(STORE(" ^ string_of_bool store ^ "), " ^ ast_of_ty t1 ^ ", " ^ "FORMALS(" ^ ast_of_bindings bs ^ "), " ^ sast_of_s_list (n + 1) s ^ ")"
  | SCall(name, args) -> "SCALL(" ^ name ^ "," ^ " ARGS(" ^ List.fold_left (fun acc ex -> (if acc = "" then acc else acc ^ ", ") ^ sast_of_sexpr n ex) "" args ^ "))"
  | SListLit(es) -> "SLIST(" ^ List.fold_left (fun acc ex -> (if acc = "" then acc else acc ^ ", ") ^ sast_of_sexpr n ex) "" es ^ ")"
  | SNoexpr -> "SNOEXPR")
and 
  sast_of_s_list n s = "[" ^ (List.fold_left (fun acc st -> acc ^ " " ^ sast_of_sstatement n st) "" s) ^ "]"
and  
  sast_of_sstatement n statement = 
    let statement_str = 
      match statement with
          SExpr(e) -> sast_of_sexpr n e
        | SAssign(v, e) -> "SASSIGN(" ^ v ^ ", " ^ sast_of_sexpr n e ^ ")"
        | SDefine(s, t, v, e) -> "SDEFINE(" ^ string_of_bool s ^ ", " ^ ast_of_ty t ^ ", " ^ v ^ ", " ^ sast_of_sexpr n e ^ ")"
        | SReturn(e) -> "SRETURN(" ^ sast_of_sexpr n e ^ ")"
        | SIf(e, s1, s2) -> "SIF(" ^ sast_of_sexpr n e ^ ", " ^ sast_of_s_list (n + 1) s1 ^ ", " ^ sast_of_s_list (n + 1) s2 ^ ")"
        | SWhile(e, s) -> "SWHILE(" ^ sast_of_sexpr n e ^ ", " ^ sast_of_s_list (n + 1 ) s ^ ")"
    in 
    "\n" ^ n_tabs n ^ statement_str
and n_tabs n = 
  if n = 0 then "" else "    " ^ (n_tabs (n - 1))
  
let sast_of_sprogram = function
  | SProgram(statements) -> "SPROGRAM {" ^ sast_of_s_list 1 statements  ^ "}"
