%{

open Types
;;
open Comm
;;

%}

%token EOF 
%token QUIT
%token EXTR
%token OK_EXTR
%token KO_EXTR
%token JOIN

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

%token COMMA
%token COLON
%token TERM

%token BOOLT
%token INTT
%token FLOATT
%token STRINGT

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
| EXTR ty_par COMMA ty_par { Extract($2,$4) }
| OK_EXTR ty_par COMMA ty_par { OKextract($2,$4) }
| KO_EXTR ty_par COMMA ty_par { KOextract($2,$4) }
| JOIN LPAR2 vars RPAR2 COMMA ty_par COMMA LPAR2 map RPAR2 COMMA INT { Join($3,$6,$9,$12) }

ty_par:
  ty_seq              { $1 }
| ty_par PAR ty_seq   { ParTy($1,$3) }

ty_seq:
  ty_bas              { $1 }
| ty_seq SEQ ty_bas { SeqTy($1,$3) }

ty_bas:
   ID { BasicTy($1) }
| INT { Var($1) }
| STOPT { SkipTy }
| LPAR ty_par RPAR { $2 }

vars:
| INT { [$1] }
| INT COMMA vars {$1::$3}

map:
| INT ARROW ty_par { [($1,$3)] }
| INT ARROW ty_par COMMA map { ($1,$3)::$5 }
