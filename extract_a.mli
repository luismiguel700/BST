open Assertions;;

exception Fail of string;;

type map = (int * assertion) list (* optimizar mais tarde com hashmaps *)

val init: assertion -> assertion -> unit;;

val hasNext: unit -> bool;;

val next: unit -> (assertion*assertion*map)
