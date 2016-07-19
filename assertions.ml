open List;;
open Types;;

type assertion =
| Skip
| Hole of int
| Var of int
| Basic of string * ty
| Seq of assertion * assertion
| Par of assertion * assertion
;;

let rec print_assertion_lev t l = 
	match t with
	| Skip -> print_string "0"
	| Hole(id) -> print_string ("0_"^(string_of_int id))
	| Var(id) -> print_int id
	| Basic(id, t) -> 
		print_string id;
		print_string ":";
		print_type t;
	| Seq(a, b) -> 
		print_assertion_lev a 1;
		print_string ";";
		print_assertion_lev b 1;
	| Par(a,b) -> 
		if (l>0) then 
			print_string "(";
		print_assertion_lev a 0;
		print_string "|";
		print_assertion_lev b 0;
		if (l>0) then
			print_string ")"
;;

let print_assertion t = print_assertion_lev t 0;;

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

(* A=A'[x] ? *) (* optmimizar mais tarde *)
let rec containsVars a xs =
	match a with
	| Skip -> false
	| Hole(_) -> false
	| Var(id) -> mem id xs
	| Basic(_, _) -> false
	| Seq(a1,a2) -> containsVars a1 xs || containsVars a2 xs
	| Par(a1,a2) -> containsVars a1 xs || containsVars a2 xs

let rec consistsOfVars a =
	match a with
	| Skip -> false
	| Hole(_) -> false
	| Var(_) -> true
	| Basic(_, _) -> false
	| Seq(a1,a2) -> consistsOfVars a1 && consistsOfVars a2
	| Par(a1,a2) -> consistsOfVars a1 && consistsOfVars a2

(* A{C/b} *)
let rec subst a b c =
	match a with
	| Skip -> a
	| Hole(_) -> if a=b then c else a
	| Var(_) -> if a=b then c else a
	| Basic(_, _) -> if a=b then c else a
	| Seq(a1,a2) -> Seq(subst a1 b c, subst a2 b c)
	| Par(a1,a2) -> Par(subst a1 b c, subst a2 b c)