open Assertions;;

type map = (int * assertion) list (* optimizar mais tarde com hashmaps *)

val extr: assertion -> assertion -> ((assertion*assertion*map) -> unit) -> unit;;

val init: assertion -> assertion -> ((assertion*assertion*map) -> unit) -> unit;;

val hasNext: unit -> bool;;

val next: unit -> unit;;
