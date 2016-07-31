open Types;;
open Assertions;;
open Exp;;

val print_type: ty -> unit;;

val print_assertion: assertion -> unit;;

val print_exp: exp -> unit;;

val print_list : ('a -> unit ) -> 'a list -> unit ;;