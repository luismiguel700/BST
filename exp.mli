open Types;;

type exp =
| Id of int
| Fun of int * ty * exp
| Call of exp * exp
| Let of int * exp * exp
| Select of exp * int
;;
