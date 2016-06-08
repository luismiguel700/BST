type ty =
     | BasicTy of string
     | SkipTy
     | SeqTy of ty * ty
     | ParTy of ty * ty;;

exception FailId of string*string;;
exception FailEmptyRes;;
exception Fail of string;;

val print_type: ty -> unit;;

val extr: ty -> ty -> (ty*ty);;
