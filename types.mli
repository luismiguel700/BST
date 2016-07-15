open List;;

type ty = 
| BasicTy of string
| SkipTy
| SeqTy of ty * ty
| ParTy of ty * ty
| Var of int
| Hole of int
;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

exception VarNotFound of int;;
exception VarsNotFound of int list;;

val print_type: ty -> unit;;

val resetCount: unit -> unit;;

val freshId: unit -> int;;

val isSkip: ty -> bool;;

val inFst: ty -> ty -> bool;;

val consistsOfVars: ty -> bool;;

val substVarsHoles: ty -> map -> ty;;

val substHolesVars: ty -> map -> ty;;

val join: (int list) -> ty -> map -> int -> (ty*(int*ty))
