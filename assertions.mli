open List;;
open Types;;
open Exceptions;;

type assertion =
| Skip
| Hole of int
| Var of int
| Basic of int * ty
| Seq of assertion * assertion
| Par of assertion * assertion
;;

val makeAssertion: int -> ty -> assertion;;

val makeCanonical: assertion -> assertion;;

val isSkip: assertion -> bool;;

val maySkip: assertion -> bool;;


(* val inFst: assertion -> assertion -> bool;; *)

val inFst_act: int -> assertion -> bool;;

val containsVars: assertion -> int list -> bool;;

val consistsOfVars: assertion -> int list -> bool;;

val consistsOfVarsEnv: assertion -> (int*assertion) list -> bool;;

val subst: assertion -> assertion -> assertion -> assertion;;

val mkPar: assertion -> assertion -> assertion;;

val mkSeq: assertion -> assertion -> assertion;;
