open Types;;
open Assertions;;
open Exp;;
open Extract;;
open Extract_a;;

val typecheck: assertion -> exp -> ty -> ((assertion*Extract_a.map)->unit) -> unit;;

val init: assertion -> exp -> ty -> ((assertion*Extract_a.map) -> unit) -> unit;;

val hasNext: unit -> bool;;

val next: unit -> unit;;
