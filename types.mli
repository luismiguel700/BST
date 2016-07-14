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

exception Fail of string;;
exception VarNotFound of int;;
exception VarsNotFound of int list;;

val print_type: ty -> unit;;

val resetCount: unit -> unit;;

val freshId: unit -> int;;

val isSkip: ty -> bool;;

val substVarsHoles: ty -> map -> ty;;

val substHolesVars: ty -> map -> ty;;

val join: (int list) -> ty -> map -> int -> (ty*(int*ty))

val extract: ty -> ty -> ((ty * ty * map) list);;
