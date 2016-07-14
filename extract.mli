open Types;;

exception Fail of string;;

val init: ty -> ty -> unit;;

val hasNext: unit -> bool;;

val next: unit -> (ty*ty*map)
