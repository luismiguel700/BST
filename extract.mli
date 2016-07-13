open Types;;

exception Fail of string;;

val extract: ty -> ty -> ty*ty*map;;
