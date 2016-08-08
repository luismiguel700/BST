open Types;;
open Assertions;;
open List;;
open Unparser;;

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

let rec promote(a:assertion) xs =
	match a with
	| Skip -> a
	| Hole(_) ->a
	| Var(id) -> if mem id xs then Skip else a
	| Basic(_, _) -> a
	| Seq(a1, a2) -> let a1' = promote a1 xs in
	                   if isSkip a1' then promote a2 xs
                                         else mkSeq a1' a2
	| Par(a1, a2) -> mkPar (promote a1 xs) (promote a2 xs)

let rec join_ass(xs:int list)(a:assertion) y =
	match a with
	| Skip -> (a,a)
	| Hole(_) -> (Skip,a)
	| Var(id) -> if (mem id xs) then (Var(y), Skip) else (Skip, a)	
	| Basic(_, _) -> (Skip, a)
	| Seq(a1, a2) ->
		 if (isSkip a1) then join_ass xs a2 y else 
	                 let (a1',a1'') = join_ass xs a1 y
			 in 
                         if (isSkip a2) then (a1',a1'')
			 else if isSkip a1' then (Skip, mkSeq a1'' a2) 
                         else (if a1'=Var(y) && isSkip a1'' then 
				  let (a2',a2'') = join_ass xs a2 y in
				     (mkSeq a1' (promote ( mkPar a2' a2'') [y]), Skip)
                              else  (mkSeq (mkPar a1' a1'') a2, Skip))
	| Par(a1, a2) -> let (a1',a1'') = join_ass xs a1 y in
			 let (a2',a2'') = join_ass xs a2 y in (
                         ( mkSeq (Var(y)) (promote (mkPar a1' a2') [y]), mkPar a1'' a2'' ) )

let rec join_as(xs:int list)(a:assertion)(y:int) =
        let (a1,a2) = join_ass xs a y in mkPar a1 a2
