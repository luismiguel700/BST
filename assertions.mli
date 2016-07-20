open List;;
open Types;;

type assertion =
| Skip
| Hole of int
| Var of int
| Basic of int * ty
| Seq of assertion * assertion
| Par of assertion * assertion
;;

val isSkip: assertion -> bool;;

val inFst: assertion -> assertion -> bool;;

val consistsOfVars: assertion -> bool;;

val subst: assertion -> assertion -> assertion -> assertion;;
