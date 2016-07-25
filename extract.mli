open Types;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

val s: ( ((some ref) Stack.t) * (unit -> unit) ) Stack.t;;

val idCount: int ref;;

val resetCount: unit -> unit;;

val freshId: unit -> int;;

val extr: ty -> ty -> ((some ref) Stack.t) list -> ((ty*ty*map) -> unit) -> unit;;

val init: ty -> ty -> ((ty*ty*map)->unit) -> unit;;

val hasNext: unit -> bool;;

val next: unit -> unit;;
