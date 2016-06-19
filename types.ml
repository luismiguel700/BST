type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty
	| Var of string
;;

exception FailId of string*string;;
exception FailEmptyRes;;
exception Fail of string;;
exception NoVar of string;;
exception FailJoin of string * string * ty;;

let rec print_type_lev t l = 
  match t with
  |  SkipTy -> 
       print_string "0"
  |  BasicTy(id) -> 
       print_string id;
  |  Var(id) ->
       print_string id;
  |  SeqTy(a,b) -> 
       print_type_lev a 1;
       print_string ";";
	  print_type_lev b 1;
  |  ParTy(a,b) -> 
       if (l>0) then print_string "(";
       print_type_lev a 0;
       print_string "|";
	  print_type_lev b 0;
       if (l>0) then print_string ")"
;;

let print_type t = print_type_lev t 0;;

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| SkipTy -> true
	| BasicTy(_) -> false
	| Var(_) -> false
	| SeqTy(a1,a2) -> isSkip a1 && isSkip a2
	| ParTy(a1,a2) -> isSkip a1 && isSkip a2

(* exists(C, A=C[t]) *)
let rec inFst t a = 
	match a with
	| SkipTy -> false
	| BasicTy(_) -> t=a
	| Var(_) -> t=a
	| SeqTy(a1,a2) -> 
		if isSkip a then
			inFst t a2
		else
			inFst t a1
	| ParTy(a1,a2) -> inFst t a1 || inFst t a2 
;;

(* split x A[x] = (A', A'') ==> A[x] <: x;A' | A'' *)
let rec split a x =
	match a with
	| SkipTy -> raise (NoVar(x))
	| Var(id) when id=x -> (SkipTy, SkipTy)
	| Var(id) -> raise (NoVar(x))
	| BasicTy(id) -> raise (NoVar(id))
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
			raise (NoVar(x))

(* join_par y z A[y][z] x = A'[x] where A[y][z] <: A'[y|z] *)
let rec join_par y z a x =
	match a with
	| SkipTy -> raise (FailJoin(y,z,a))
	| Var(id) -> raise (FailJoin(y,z,a))
	| BasicTy(id) -> raise (FailJoin(y,z,a))
	| SeqTy(b, c) ->
		let b' = join_par y z b x in
			SeqTy(b', c)
	| ParTy(b, c) ->
		if inFst (Var(y)) b && inFst (Var(z)) c then
		(
			let (b', b'') = split b y in
			let (c', c'') = split c z in
				(ParTy(SeqTy(Var(x), ParTy(b', c')), ParTy(b'', c'')))
		)
		else if inFst (Var(y)) c && inFst (Var(z)) b then
		(
			let (b', b'') = split b z in
			let (c', c'') = split c y in
				(ParTy(SeqTy(Var(x), ParTy(b', c')), ParTy(b'', c'')))
		)
		else if inFst (Var(y)) b && inFst (Var(z)) b then
			join_par y z b x
		else if inFst (Var(y)) c && inFst (Var(z)) c then
			join_par y z c x
		else
			raise (FailJoin(y,z,a))

let rec extr a b = 
	match a,b with
	| _, SkipTy -> (a,b)
	| SkipTy, BasicTy(id) -> (a, b)
	| BasicTy(id), BasicTy(id') when id=id' -> (SkipTy,SkipTy)
	| BasicTy(id), BasicTy(id') -> raise (FailId(id,id'))
	| SeqTy(a1,a2), BasicTy(id) -> extrSeqId a1 a2 b
	| ParTy(a1,a2), BasicTy(id) -> extrParId a1 a2 b
	| _, SeqTy(b1,b2) -> extrSeq a b1 b2
	| SkipTy, ParTy(_,_) -> (a,b)
	| BasicTy(id), ParTy(b1,b2) -> extrIdPar id b1 b2
	| SeqTy(a1,a2), ParTy(b1,b2) -> extrSeqPar a1 a2 b1 b2
(*	| ParTy(a1,a2), ParTy(b1,b2) -> (SkipTy, SkipTy) *)

and extrSeqId a1 a2 b = 
	let (a1', b') = extr a1 b in
		match a1', b' with
		| _, SkipTy -> (SeqTy(a1',a2), b')
		| SkipTy, _ -> extr a2 b'
		| _, _ -> raise FailEmptyRes

and extrParId a1 a2 b =
	if inFst b a1 && inFst b a2 then
		let (a1',_) = extr a1 b in
		let (a2',_) = extr a2 b in
			(ParTy(ParTy(a1',a2'), b),SkipTy)
	else if inFst b a1 then
		let (a1',_) = extr a1 b in
			(ParTy(a1',a2),SkipTy)
	else if inFst b a2 then
		let (a2',_) = extr a2 b in
			(ParTy(a1,a2'),SkipTy)
	else
		let (a1',b) = extr a1 b in
		let (a2',c) = extr a2 b in
			match a1',a2' with
			| SkipTy, SkipTy -> (SkipTy, b)
			| _, _ -> raise FailEmptyRes

and extrSeq a b1 b2 =
	let (a', b1') = extr a b1 in
		match a', b1' with
		| SkipTy, _ -> (a', SeqTy(b1',b2))
		| _, SkipTy -> extr a' b2
		| _, _ -> raise FailEmptyRes

and extrIdPar id b1 b2 =
	if isSkip b1 && isSkip b2 then
		(BasicTy(id), SkipTy)
	else if isSkip b1 then
		extr (BasicTy(id)) b2
	else if isSkip b2 then
		extr (BasicTy(id)) b1
	else
		raise (Fail("extrIdPar"))

and extrSeqPar a1 a2 b1 b2 =
	let (a1', b') = extr a1 (ParTy(b1,b2)) in
		match b' with
		| SkipTy -> (SeqTy(a1',a2), SkipTy)
		| _ -> 
			if isSkip b2 then
				extr (SeqTy(a1,a2)) b1
			else if isSkip b1 then
				extr (SeqTy(a1,a2)) b2
			else
				raise (Fail("extrSeqPar"))
;;
