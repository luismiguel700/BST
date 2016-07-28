open List;;

type ty =
| SkipTy
| HoleTy of int
| VarTy of int
| SomeTy of some ref
| FunTy of ty * ty
| BasicTy of int
| SeqTy of ty * ty
| ParTy of ty * ty
and
some =
| None
| Some of ty
;;

val isSkip: ty -> bool;;

val inFst: ty -> ty -> bool;;

val containsVars: ty -> int list -> bool;;

val consistsOfVars: ty -> (int*'a) list -> bool;;

val subst: ty -> ty -> ty -> ty;;
