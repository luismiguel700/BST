type ty =
     | BasicTy of string
     | SkipTy
     | SeqTy of ty * ty
     | ParTy of ty * ty;;

val print_type: ty -> unit;;

