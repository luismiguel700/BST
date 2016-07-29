open Types;;
open Assertions;;
open Extract;;
open Extract_a;;

exception VarNotFound of int;;
exception VarsNotFound of int list;;

val join: (int list) -> ty -> Extract.map -> int -> (ty*(int*ty))

val join_a: (int list) -> assertion -> (Extract_a.map) -> int -> assertion
