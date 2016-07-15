open Types;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

exception VarNotFound of int;;
exception VarsNotFound of int list;;

val join: (int list) -> ty -> map -> int -> (ty*(int*ty))
