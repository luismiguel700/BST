type exp =
| Id of string
| Fun of string * exp
| Call of exp * exp
| Let of string * exp * exp
| Select of exp * string
;;

val print_exp: exp -> unit;;
