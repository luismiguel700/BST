type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty
	| Var of int
	| Hole of int
;;
type map = (int * ty) list

exception FailId of string*string;;
exception FailVarId of int*string;;
exception FailEmptyRes;;
exception Fail of string;;

val resetCount: unit -> unit;;

val print_type: ty -> unit;;

val extr: ty -> ty -> ((ty * ty * map) list);;
