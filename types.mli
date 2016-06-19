type ty =
     | BasicTy of string
     | SkipTy
     | SeqTy of ty * ty
     | ParTy of ty * ty
     | Var of string
;;

exception FailId of string*string;;
exception FailEmptyRes;;
exception Fail of string;;

val print_type: ty -> unit;;

val join_par: string -> string -> ty -> string -> ty

val split: ty -> string -> (ty*ty)

val extr: ty -> ty -> (ty*ty);;
