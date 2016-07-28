open Types;;
open Assertions;;
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
	| SomeTy(_) -> raise (VarNotFound(x))
	| FunTy(_, _) -> raise (VarNotFound(x))
	| BasicTy(_) -> raise (VarNotFound(x))
	| SeqTy(b, c) -> 
		let (a', a'') = split b x in
			(SeqTy(ParTy(a', a''), c), SkipTy)
	| ParTy(b, c) ->
		if Types.inFst (VarTy(x)) b then
			let (b', b'') = split b x in
				(b', ParTy(b'', c))
		else if Types.inFst (VarTy(x)) c then
			let (c', c'') = split c x in
				(c', ParTy(b, c''))
		else
			raise (VarNotFound(x))

(* join(~x, A[~x], H, y)=(A'[y], H')  ==>  A{H(~x)/~x} <: A'{H'(y)/y} *)
let rec join(xs: int list)(a:ty)(h:Extract.map)(y:int):(ty*(int*ty)) =
	match a with
	| SkipTy -> raise (VarsNotFound(xs))
	| HoleTy(_) -> raise (VarsNotFound(xs))
	| VarTy(id) -> if mem id xs then (VarTy(y), (y, mapFind h id)) else raise (VarsNotFound(xs))
	| SomeTy(_) -> raise (VarsNotFound(xs))
	| FunTy(_, _) -> raise (VarsNotFound(xs))	
	| BasicTy(_) -> raise (VarsNotFound(xs))
	| SeqTy(b, c) ->
		if Types.containsVars c xs then
			let (b', (y', d)) = join xs b h y in (* may not be necessary *)
			let (c', (y', e)) = join xs c h y in
				(SeqTy(VarTy(y), Types.subst c' (VarTy(y)) SkipTy), (y, SeqTy(d,e)))
		else
			let (b', h') = join xs b h y in
				(SeqTy(b', c), h')
	| ParTy(b, c) ->
		if Types.containsVars b xs && Types.containsVars c xs then
		(
			let (b', (y', d)) = join xs b h y in
			let (c', (y', e)) = join xs c h y in 
				let (b'', b''') = split b' y in
				let (c'', c''') = split c' y in
					(ParTy(ParTy(SeqTy(VarTy(y), ParTy(b'', c'')), b'''), c'''), (y, ParTy(d,e)))
		)
		else if Types.containsVars b xs then
			let (b', h') = join xs b h y in
				(ParTy(b', c), h')
		else if Types.containsVars c xs then
			let (c', h') = join xs c h y in
				(ParTy(b, c'), h')
		else
			raise (VarsNotFound(xs))

let rec split_a a x =
	match a with
	| Skip -> raise (VarNotFound(x))
	| Hole(_) -> raise (VarNotFound(x))
	| Var(id) when id=x -> (Skip, Skip)
	| Var(_) -> raise (VarNotFound(x))
	| Basic(_, _) -> raise (VarNotFound(x))
	| Seq(b, c) -> 
		let (a', a'') = split_a b x in
			(Seq(Par(a', a''), c), Skip)
	| Par(b, c) ->
		if Assertions.inFst (Var(x)) b then
			let (b', b'') = split_a b x in
				(b', Par(b'', c))
		else if Assertions.inFst (Var(x)) c then
			let (c', c'') = split_a c x in
				(c', Par(b, c''))
		else
			raise (VarNotFound(x))

let rec join_a(xs:int list)(a:assertion)(h:Extract_a.map)(y:int):(assertion*(int*assertion)) =
	match a with
	| Skip -> raise (VarsNotFound(xs))
	| Hole(_) -> raise (VarsNotFound(xs))
	| Var(id) -> if mem id xs then (Var(y), (y, mapFind h id)) else raise (VarsNotFound(xs))	
	| Basic(id, _) -> raise (VarsNotFound(xs))
	| Seq(b, c) ->
		if Assertions.containsVars c xs then
			let (b', (y', d)) = join_a xs b h y in (* may not be necessary *)
			let (c', (y', e)) = join_a xs c h y in
				(Seq(b', Assertions.subst c' (Var(y)) Skip), (y, Seq(d,e)))
		else
			let (b', h') = join_a xs b h y in
				(Seq(b', c), h')			

	| Par(b, c) ->
		if Assertions.containsVars b xs && Assertions.containsVars c xs then
			let (b', (y', d)) = join_a xs b h y in
			let (c', (y', e)) = join_a xs c h y in 
				let (b'', b''') = split_a b' y in
				let (c'', c''') = split_a c' y in
					(Par(Par(Seq(Var(y), Par(b'', c'')), b'''), c'''), (y, Par(d,e)))
		else if Assertions.containsVars b xs then
			let (b', h') = join_a xs b h y in
				(Par(b', c), h')
		else if Assertions.containsVars c xs then
			let (c', h') = join_a xs c h y in
				(Par(b, c'), h')
		else
			raise (VarsNotFound(xs))
