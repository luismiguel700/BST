open List;;

type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty
	| Var of int
	| Hole of int
;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

exception VarNotFound of int;;
exception VarsNotFound of int list;;

let idCount = ref 0;;

let resetCount () = idCount := 0

let freshId () = (idCount := (!idCount+1)); !idCount

let rec print_type_lev t l = 
	match t with
	| SkipTy -> print_string "0"
	| BasicTy(id) -> print_string id
	| Var(id) -> print_int id
	| Hole(id) -> print_string ("0_"^(string_of_int id))
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

let rec mapFind h id = 
	match h with
	| [] -> raise (VarNotFound(id))
	| (id', a)::hs -> if id' = id then a else mapFind hs id

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| SkipTy -> true
	| BasicTy(_) -> false
	| Var(_) -> false
	| Hole(_) -> true
	| SeqTy(a1,a2) -> isSkip a1 && isSkip a2
	| ParTy(a1,a2) -> isSkip a1 && isSkip a2

(* A=C[t] *)
let rec inFst t a = 
	match a with
	| SkipTy -> false
	| BasicTy(_) -> t=a
	| Var(_) -> t=a
	| Hole(_) -> t=a
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
	| BasicTy(_) -> false
	| Hole(_) -> false
	| Var(id) -> mem id xs
	| SeqTy(a1,a2) -> containsVars a1 xs || containsVars a2 xs
	| ParTy(a1,a2) -> containsVars a1 xs || containsVars a2 xs

(* A{C/b} *)
let rec subst a b c =
	match a with
	| SkipTy -> a
	| BasicTy(_) -> if a=b then c else a
	| Var(_) -> if a=b then c else a
	| Hole(_) -> if a=b then c else a
	| SeqTy(a1,a2) -> SeqTy(subst a1 b c, subst a2 b c)
	| ParTy(a1,a2) -> ParTy(subst a1 b c, subst a2 b c)

let rec substVarsHoles a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substVarsHoles (subst a (Var(id)) (Hole(id))) tail

let rec substHolesVars a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substHolesVars (subst a (Hole(id)) (Var(id))) tail

(* split A[x] x = (A', A'') ==> A[x] <: x;A' | A'' *)
let rec split a x =
	match a with
	| SkipTy -> raise (VarNotFound(x))
	| Var(id) when id=x -> (SkipTy, SkipTy)
	| Var(id) -> raise (VarNotFound(x))
	| BasicTy(id) -> raise (VarNotFound(x))
	| Hole(_) -> raise (VarNotFound(x))
	| SeqTy(b, c) -> 
		let (a', a'') = split b x in
			(SeqTy(ParTy(a', a''), c), SkipTy)
	| ParTy(b, c) ->
		if inFst (Var(x)) b then
			let (b', b'') = split b x in
				(b', ParTy(b'', c))
		else if inFst (Var(x)) c then
			let (c', c'') = split c x in
				(c', ParTy(b, c''))
		else
			raise (VarNotFound(x))

(* join(~x, A[~x], H, y)=(A'[y], H')  ==>  A{H(~x)/~x} <: A'{H'(y)/y} *)
let rec join(xs: int list)(a:ty)(h:map)(y:int):(ty*(int*ty)) =
	match a with
	| SkipTy -> raise (VarsNotFound(xs))
	| Hole(_) -> raise (VarsNotFound(xs))
	| Var(id) -> if mem id xs then (Var(y), (y, mapFind h id)) else raise (VarsNotFound(xs))
	| BasicTy(id) -> raise (VarsNotFound(xs))
	| SeqTy(b, c) ->
		if containsVars c xs then
			let (b', (y', d)) = join xs b h y in (* may not be necessary *)
			let (c', (y', e)) = join xs c h y in
				(SeqTy(Var(y), subst c' (Var(y)) SkipTy), (y, SeqTy(d,e)))
		else
			let (b', h') = join xs b h y in
				(SeqTy(b', c), h')
	| ParTy(b, c) ->
		if containsVars b xs && containsVars c xs then
		(
			let (b', (y', d)) = join xs b h y in
			let (c', (y', e)) = join xs c h y in 
				let (b'', b''') = split b' y in
				let (c'', c''') = split c' y in
					(ParTy(ParTy(SeqTy(Var(y), ParTy(b'', c'')), b'''), c'''), (y, ParTy(d,e)))
		)
		else if containsVars b xs then
			let (b', h') = join xs b h y in
				(ParTy(b', c), h')
		else if containsVars c xs then
			let (c', h') = join xs c h y in
				(ParTy(b, c'), h')
		else
			raise (VarsNotFound(xs))
