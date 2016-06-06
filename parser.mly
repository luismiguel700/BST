%{

open Types
;;
open Comm
;;

%}

%token EOF 
%token QUIT
%token EXTR

%token PAR
%token SEQ
%token AND
%token BANG
%token SUBT

%token ID
%token STOPT

%token LPAR
%token RPAR

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

ty_par:
  ty_seq              { $1 }
| ty_par PAR ty_seq   { ParTy($1,$3) }

ty_seq:
  ty_bas              { $1 }
| ty_seq SEQ ty_bas { SeqTy($1,$3) }

ty_bas:
   ID { BasicTy($1) }
| STOPT { SkipTy }
| LPAR ty_par RPAR { $2 }


