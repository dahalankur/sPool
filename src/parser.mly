%{
open Ast
%}

%token PLUS MINUS TIMES DIVIDE NOT EQ NEQ LT LEQ GT GEQ AND OR MOD 
%token ASSIGN ELSE IF WHILE
%token INT BOOL FLOAT QUACK MUTEX THREAD STRING LIST ARROW
%token DEF STORE RETURN LAMBDA
%token NEWLINE LPAREN RPAREN LBRACE RBRACE COLON SEMI COMMA LSQUARE RSQUARE EOF
%token <int> LITERAL
%token <bool> BLIT
%token <string> NAME
%token <string> STRINGLIT
%token <string> FLIT

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
    d_opt statement_list EOF { Program($2) }

d_opt:
    /* nothing */  { }
  | delimiter      { }

store_opt:
    /* nothing */ { false }
  | STORE         { true  }

args_opt:
    /* nothing */ { []          }
  | args_list     { List.rev $1 }

args_list:
    expr                    { [$1]     }
  | args_list COMMA expr    { $3 :: $1 }

formals_opt:
    /* nothing */ { []          }
  | formal_list   { List.rev $1 }

formal_list:
    typ NAME                   { [($1,$2)]     }
  | formal_list COMMA typ NAME { ($3,$4) :: $1 }

delimiter:
    NEWLINE           {}
  | delimiter NEWLINE {}

typ_list:
    typ                       { [$1]     }
  | typ_list COMMA typ        { $3 :: $1 }

typ:
     INT                              { Int                    }
   | BOOL                             { Bool                   }
   | QUACK                            { Quack                  }
   | FLOAT                            { Float                  }
   | STRING                           { String                 }
   | MUTEX                            { Mutex                  }
   | THREAD                           { Thread                 }
   | LIST LT typ GT                   { List($3)               }
   | LPAREN typ_list ARROW typ RPAREN { Arrow(List.rev $2, $4) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1     }

statement_list:
    /* nothing */                       { []       }
  | statement delimiter statement_list  { $1 :: $3 }
  | statement                           { [$1]     }

statement:
      expr                 { Expr($1)           }
    | typ NAME ASSIGN expr { Define($1, $2, $4) }
    | NAME ASSIGN expr     { Assign($1, $3)     } 
    | RETURN expr_opt      { Return($2)         } 
    | DEF store_opt typ NAME LPAREN formals_opt RPAREN COLON d_opt statement_list SEMI 
                           { FunDef($2, $3, $4, $6, $10) }    
    | WHILE LPAREN expr RPAREN COLON d_opt statement_list SEMI { While($3, $7)            }
    | IF LPAREN expr RPAREN COLON d_opt statement_list SEMI    { If($3, $7, [])           }
    | IF LPAREN expr RPAREN COLON d_opt statement_list ELSE d_opt statement_list SEMI    
                                                               { If($3, $7, $10) }

expr:
    FLIT	           {      Fliteral($1)      }
  | BLIT             {      BoolLit($1)       }
  | LITERAL          {      Literal($1)       }
  | STRINGLIT        {      StringLiteral($1) }
  | NAME             {      Var($1)           }
  | LSQUARE args_opt RSQUARE   { ListLit($2)  }
  | LBRACE d_opt statement_list RBRACE { Thread($3) }
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
  | NOT expr         { Unop(Not, $2)          }
  | NAME LPAREN args_opt RPAREN { Call($1, $3)}
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | LAMBDA typ LPAREN formals_opt RPAREN COLON d_opt statement_list SEMI  { Lambda($2, $4, $8) }
  | LPAREN expr RPAREN { $2 }
