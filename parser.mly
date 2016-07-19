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
| EXTR ty_par ty_par { Extract($2, $3) }
| EXTR_A par par { ExtractA($2, $3) }
| OK_EXTR ty_par ty_par { OKextract($2, $3) }
| KO_EXTR ty_par ty_par { KOextract($2, $3) }
| JOIN join_args { let (xs, a, h, y) = $2 in Join(xs, a, h, y) }
| OK_JOIN join_args ty_par { let (xs, a, h, y) = $2 in OKjoin(xs, a, h, y, $3) }
| TYPE par LPAR exp RPAR ty_par { Typecheck($2, $4, $6) } 

/******** TYPES ********/

ty_par:
  ty_seq              { $1 }
| ty_par PAR ty_seq   { ParTy($1,$3) }

ty_seq:
  ty_fun              { $1 }
| ty_seq SEQ ty_fun { SeqTy($1,$3) }

ty_fun:
| ty_bas { $1 }
| ty_fun ARROW ty_bas { FunTy($1, $3) }

ty_bas:
| STOPT { SkipTy }
| NAT { VarTy($1) }
| ID { BasicTy($1) }
| LPAR ty_par RPAR { $2 }

/***********************/

/***** ASSERTIONS *****/

par:
  seq              { $1 }
| par PAR seq   { Par($1,$3) }

seq:
  bas              { $1 }
| seq SEQ bas { Seq($1,$3) }

bas:
| STOPT { Skip }
| NAT { Var($1) }
| ID COLON ty_fun { Basic($1, $3) }
| LPAR par RPAR { $2 }

/***********************/

/***** EXPRESSIONS *****/

exp:
| call_exp				{ $1 }
| FUN ID ARROW exp 		{ Fun($2, $4) }
| LET ID EQ exp IN exp 	{ Let($2, $4, $6) }

call_exp:
| id_exp				{ $1 }
| call_exp id_exp		{ Call($1, $2) }

id_exp:
| ID 					{ Id($1) }
| id_exp DOT ID 		{ Select($1, $3) }
| LPAR exp RPAR 		{ $2 }

/***********************/

vars:
| NAT { [$1] }
| NAT COMMA vars {$1::$3}

map:
| NAT ARROW ty_par { [($1,$3)] }
| NAT ARROW ty_par COMMA map { ($1,$3)::$5 }

join_args:
	LPAR2 vars RPAR2 ty_par LPAR2 map RPAR2 NAT { ($2,$4,$6,$8) } 
