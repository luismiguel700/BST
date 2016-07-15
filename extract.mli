open Types;;

exception Fail of string;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

val init: ty -> ty -> unit;;

val hasNext: unit -> bool;;

val next: unit -> (ty*ty*map)
