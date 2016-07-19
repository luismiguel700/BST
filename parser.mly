%{

open Types
;;
open Assertions
;;
open Exp
;;
open Comm
;;

%}

%token EOF 
%token QUIT
%token EXTR
%token EXTR_A
%token OK_EXTR
%token KO_EXTR
%token JOIN
%token OK_JOIN
%token TYPE

%token PAR
%token SEQ
%token AND
%token BANG
%token SUBT

%token ID
%token STOPT

%token LPAR
%token RPAR
%token LPAR2
%token RPAR2
%token ARROW
%token EQ

%token DOT
%token COMMA
%token COLON
%token DOUBLE_COLON
%token TERM

%token LET
%token FUN
%token IN

%token BOOLT
%token INTT
%token FLOATT
%token STRINGT

%token <int> NAT
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%type <Comm.comm> main

%start main


%%

main:
command TERM { $1 }

command:
| QUIT { Quit }
| EXTR ty ty { Extract($2, $3) }
| EXTR_A assertion assertion { ExtractA($2, $3) }
| OK_EXTR ty ty { OKextract($2, $3) }
| KO_EXTR ty ty { KOextract($2, $3) }
| JOIN join_args { let (xs, a, h, y) = $2 in Join(xs, a, h, y) }
| OK_JOIN join_args ty { let (xs, a, h, y) = $2 in OKjoin(xs, a, h, y, $3) }
| TYPE assertion LPAR exp RPAR ty { Typecheck($2, $4, $6) } 

/******** TYPES ********/

ty:
  ty2              { $1 }
| ty PAR ty2   { ParTy($1,$3) }

ty2:
  ty3              { $1 }
| ty2 SEQ ty3 { SeqTy($1,$3) }

ty3:
| ty_basic { $1 }
| ty3 ARROW ty_basic { FunTy($1, $3) }

ty_basic:
| STOPT { SkipTy }
| NAT { VarTy($1) }
| ID { BasicTy($1) }
| LPAR ty RPAR { $2 }

/***********************/

/***** ASSERTIONS *****/

assertion:
  assertion2              	{ $1 }
| assertion PAR assertion2	{ Par($1,$3) }

assertion2:
  assertion_basic              		{ $1 }
| assertion2 SEQ assertion_basic	{ Seq($1,$3) }

assertion_basic:
| STOPT { Skip }
| NAT { Var($1) }
| ID COLON ty3 { Basic($1, $3) }
| LPAR assertion RPAR { $2 }

/***********************/

/***** EXPRESSIONS *****/

exp:
| exp2				{ $1 }
| FUN ID ARROW exp 		{ Fun($2, $4) }
| LET ID EQ exp IN exp 	{ Let($2, $4, $6) }

exp2:
| exp_basic				{ $1 }
| exp2 exp_basic		{ Call($1, $2) }

exp_basic:
| ID 					{ Id($1) }
| exp_basic DOT ID 		{ Select($1, $3) }
| LPAR exp RPAR 		{ $2 }

/***********************/

vars:
| NAT { [$1] }
| NAT COMMA vars {$1::$3}

map:
| NAT ARROW ty { [($1,$3)] }
| NAT ARROW ty COMMA map { ($1,$3)::$5 }

join_args:
	LPAR2 vars RPAR2 ty LPAR2 map RPAR2 NAT { ($2,$4,$6,$8) } 
