open Types;;

exception Fail of string;;
exception FailBasic of string * string;;
exception FailVarBasic of int * string;;

val extract: ty -> ty -> ty*ty*map;;
