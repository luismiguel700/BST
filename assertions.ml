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

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| Skip -> true
	| Hole(_) -> true
	| Var(_) -> false
	| Basic(_, t) -> Types.isSkip t
	| Seq(a1,a2) -> isSkip a1 && isSkip a2
	| Par(a1,a2) -> isSkip a1 && isSkip a2

(* A=C[t] *)
let rec inFst t a = 
	match a with
	| Skip -> false
	| Hole(_) -> t=a
	| Var(_) -> t=a
	| Basic(_, _) -> t=a
	| Seq(a1,a2) -> 
		if isSkip a1 then
			inFst t a2
		else
			inFst t a1
	| Par(a1,a2) -> inFst t a1 || inFst t a2 
;;

(* A=A'[~x] ? *) (* optmimizar mais tarde *)
let rec containsVars a xs =
	match a with
	| Skip -> false
	| Hole(_) -> false
	| Var(id) -> mem id xs
	| Basic(_, _) -> false
	| Seq(a1,a2) -> containsVars a1 xs || containsVars a2 xs
	| Par(a1,a2) -> containsVars a1 xs || containsVars a2 xs

let rec consistsOfVars a vars =
	match a with
	| Skip -> true
	| Hole(_) -> false
	| Var(id) -> exists (fun (id', _) -> id=id') vars
	| Basic(_, _) -> false
	| Seq(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars
	| Par(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars

(* A{C/b} *)
let rec subst a b c =
	match a with
	| Skip -> a
	| Hole(_) -> if a=b then c else a
	| Var(_) -> if a=b then c else a
	| Basic(_, _) -> if a=b then c else a
	| Seq(a1,a2) -> Seq(subst a1 b c, subst a2 b c)
	| Par(a1,a2) -> Par(subst a1 b c, subst a2 b c)
