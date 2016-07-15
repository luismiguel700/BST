open List;;

type ty = 
| SkipTy
| HoleTy of int
| VarTy of int
| FunTy of ty * ty
| BasicTy of string
| SeqTy of ty * ty
| ParTy of ty * ty
;;

val print_type: ty -> unit;;

val isSkip: ty -> bool;;

val inFst: ty -> ty -> bool;;

val containsVars: ty -> int list -> bool;;

val consistsOfVars: ty -> bool;;

val subst: ty -> ty -> ty -> ty;;
