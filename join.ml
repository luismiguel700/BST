open Types;;
open List;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

exception VarNotFound of int;;
exception VarsNotFound of int list;;

let rec mapFind h id = 
	match h with
	| [] -> raise (VarNotFound(id))
	| (id', a)::hs -> if id' = id then a else mapFind hs id

(* split A[x] x = (A', A'') ==> A[x] <: x;A' | A'' *)
let rec split a x =
	match a with
	| SkipTy -> raise (VarNotFound(x))
	| HoleTy(_) -> raise (VarNotFound(x))
	| VarTy(id) when id=x -> (SkipTy, SkipTy)
	| VarTy(_) -> raise (VarNotFound(x))
	| FunTy(_, _) -> raise (VarNotFound(x))
	| BasicTy(_) -> raise (VarNotFound(x))
	| SeqTy(b, c) -> 
		let (a', a'') = split b x in
			(SeqTy(ParTy(a', a''), c), SkipTy)
	| ParTy(b, c) ->
		if inFst (VarTy(x)) b then
			let (b', b'') = split b x in
				(b', ParTy(b'', c))
		else if inFst (VarTy(x)) c then
			let (c', c'') = split c x in
				(c', ParTy(b, c''))
		else
			raise (VarNotFound(x))

(* join(~x, A[~x], H, y)=(A'[y], H')  ==>  A{H(~x)/~x} <: A'{H'(y)/y} *)
let rec join(xs: int list)(a:ty)(h:map)(y:int):(ty*(int*ty)) =
	match a with
	| SkipTy -> raise (VarsNotFound(xs))
	| HoleTy(_) -> raise (VarsNotFound(xs))
	| VarTy(id) -> if mem id xs then (VarTy(y), (y, mapFind h id)) else raise (VarsNotFound(xs))
	| FunTy(_, _) -> raise (VarsNotFound(xs))	
	| BasicTy(_) -> raise (VarsNotFound(xs))
	| SeqTy(b, c) ->
		if containsVars c xs then
			let (b', (y', d)) = join xs b h y in (* may not be necessary *)
			let (c', (y', e)) = join xs c h y in
				(SeqTy(VarTy(y), subst c' (VarTy(y)) SkipTy), (y, SeqTy(d,e)))
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
					(ParTy(ParTy(SeqTy(VarTy(y), ParTy(b'', c'')), b'''), c'''), (y, ParTy(d,e)))
		)
		else if containsVars b xs then
			let (b', h') = join xs b h y in
				(ParTy(b', c), h')
		else if containsVars c xs then
			let (c', h') = join xs c h y in
				(ParTy(b, c'), h')
		else
			raise (VarsNotFound(xs))
