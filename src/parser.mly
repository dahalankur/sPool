%{
open Ast
%}

// %token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
// %token RETURN IF ELSE FOR  

%token PLUS MINUS TIMES DIVIDE NOT EQ NEQ LT LEQ GT GEQ AND OR MOD ASSIGN NEWLINE ELSE IF LPAREN RPAREN COLON SEMI
%token INT BOOL FLOAT QUACK WHILE
%token <int> LITERAL
%token <bool> BLIT
%token <string> NAME
%token <string> STRING // TODO: not finalized
%token <string> FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%%

program: 
      d_opt statement_list EOF    { Program(List.rev $2) }

d_opt:
                { }
  | delimiter   { }

delimiter:
      NEWLINE           {}
    | delimiter NEWLINE {}

typ:
     INT   { Int   }
   | BOOL  { Bool  }
   | FLOAT { Float }
   | QUACK { Quack  }
// TODO: handle strings and lists later


statement_list:
    /* nothing */                       { [] }
  | statement delimiter statement_list  { $1 :: $3 }
  | statement                           { [$1] }


statement:
      expr                 { Expr($1)       }
    | typ NAME ASSIGN expr { Define($1, $2, $4) } 
    | NAME ASSIGN expr     { Assign($1, $3) } 
    | WHILE LPAREN expr RPAREN COLON d_opt statement_list SEMI { While($3, List.rev $7) }
    | IF LPAREN expr RPAREN COLON d_opt statement_list SEMI { If($3, List.rev $7, []) }
    | IF LPAREN expr RPAREN COLON d_opt statement_list ELSE d_opt statement_list SEMI    { If($3, List.rev $7, List.rev $10) }
    //(* TODO: add types because it is statically typed as another rule for initialization *)

//  
//  if (x < 2): 
//      1 + 2
//      if (x > 4):
//          5 + 2
// ; else
//      2 + 1;
//  
//    VS. 
//
//  if (x < 2): 
//      1 + 2
//      if (x > 4):
//          5 + 2
//      else
//           2 + 1;;
// 
// decls:
//    /* nothing */ { ([], [])               }
//  | decls vdecl { (($2 :: fst $1), snd $1) }
//  | decls fdecl { (fst $1, ($2 :: snd $1)) }

// fdecl:
//    typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
//      { { typ = $1;
// 	 fname = $2;
// 	 formals = List.rev $4;
// 	 locals = List.rev $7;
// 	 body = List.rev $8 } }

// formals_opt:
//     /* nothing */ { [] }
//   | formal_list   { $1 }

// formal_list:
//     typ ID                   { [($1,$2)]     }
//   | formal_list COMMA typ ID { ($3,$4) :: $1 }

// vdecl_list:
//     /* nothing */    { [] }
//   | vdecl_list vdecl { $2 :: $1 }

// vdecl:
//    typ ID SEMI { ($1, $2) }

// stmt_list:
//     /* nothing */  { [] }
//   | stmt_list stmt { $2 :: $1 }

// stmt:
//   | RETURN expr_opt SEMI                    { Return $2             }
//   | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
//                                             { For($3, $5, $7, $9)   }
//   | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
//


// expr_opt:
//     /* nothing */ { Noexpr }
//   | expr          { $1 }

expr:
    FLIT	           {      Fliteral($1)      }
  | BLIT             {      BoolLit($1)       }
  | LITERAL          {      Literal($1)       }
  | STRING           {      StringLiteral($1) }
  | NAME             {      Var($1)           }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
//   | ID LPAREN args_opt RPAREN { Call($1, $3)  }
//   | LPAREN expr RPAREN { $2                   }

// args_opt:
//     /* nothing */ { [] }
//   | args_list  { List.rev $1 }

// args_list:
//     expr                    { [$1] }
//   | args_list COMMA expr { $3 :: $1 }
