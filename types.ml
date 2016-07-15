open List;;

type ty = 
| SkipTy
| HoleTy of int
| VarTy of int
| FunTy of ty * ty
| BasicTy of string
| SeqTy of ty * ty
| ParTy of ty * ty
;;

let rec print_type_lev t l = 
	match t with
	| SkipTy -> print_string "0"
	| HoleTy(id) -> print_string ("0_"^(string_of_int id))
	| VarTy(id) -> print_int id
	| FunTy(a, b) -> 
		print_type_lev a 1;
		print_string "->";
		print_type_lev b 1;
	| BasicTy(id) -> print_string id
	| SeqTy(a, b) -> 
		print_type_lev a 1;
		print_string ";";
		print_type_lev b 1;
	| ParTy(a,b) -> 
		if (l>0) then 
			print_string "(";
		print_type_lev a 0;
		print_string "|";
		print_type_lev b 0;
		if (l>0) then
			print_string ")"
;;

let print_type t = print_type_lev t 0;;

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| SkipTy -> true
	| HoleTy(_) -> true
	| VarTy(_) -> false
	| FunTy(_) -> false
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> isSkip a1 && isSkip a2
	| ParTy(a1,a2) -> isSkip a1 && isSkip a2

(* A=C[t] *)
let rec inFst t a = 
	match a with
	| SkipTy -> false
	| HoleTy(_) -> t=a
	| VarTy(_) -> t=a
	| FunTy(_, _) -> t=a
	| BasicTy(_) -> t=a
	| SeqTy(a1,a2) -> 
		if isSkip a1 then
			inFst t a2
		else
			inFst t a1
	| ParTy(a1,a2) -> inFst t a1 || inFst t a2 
;;

(* A=A'[x] ? *) (* optmimizar mais tarde *)
let rec containsVars a xs =
	match a with
	| SkipTy -> false
	| HoleTy(_) -> false
	| VarTy(id) -> mem id xs
	| FunTy(_, _) -> false
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> containsVars a1 xs || containsVars a2 xs
	| ParTy(a1,a2) -> containsVars a1 xs || containsVars a2 xs

let rec consistsOfVars a =
	match a with
	| SkipTy -> false
	| HoleTy(_) -> false
	| VarTy(_) -> true
	| FunTy(_, _) -> false
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> consistsOfVars a1 && consistsOfVars a2
	| ParTy(a1,a2) -> consistsOfVars a1 && consistsOfVars a2

(* A{C/b} *)
let rec subst a b c =
	match a with
	| SkipTy -> a
	| HoleTy(_) -> if a=b then c else a
	| VarTy(_) -> if a=b then c else a
	| FunTy(_, _) -> if a=b then c else a
	| BasicTy(_) -> if a=b then c else a
	| SeqTy(a1,a2) -> SeqTy(subst a1 b c, subst a2 b c)
	| ParTy(a1,a2) -> ParTy(subst a1 b c, subst a2 b c)
