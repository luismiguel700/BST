open Types;;

type exp =
| Id of int
| Fun of int * ty * ty * exp
| Call of exp * exp
| Let of int * ty * exp * exp
| Select of exp * int
;;

val substId: exp -> int -> int -> exp;;
