open Types;;
open Assertions;;
open Exp;;

type map = (int * ty) list;;

type comm =
| Quit
| Extract of ty * ty
| ExtractA of assertion * assertion
| OKextract of ty * ty
| KOextract of ty * ty
| Join of (int list) * ty * map * int 
| OKjoin of (int list) * ty * map * int * ty 
| Typecheck of assertion * exp * ty
| DefineType of int * ty
;;
