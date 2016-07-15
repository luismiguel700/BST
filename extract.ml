open Types;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

exception Fail of string;;

let s:((unit -> ty*ty*map) Stack.t) ref = ref (Stack.create ());;

let idCount = ref 0;;

let resetCount () = idCount := 0

let freshId () = (idCount := (!idCount+1)); !idCount

let rec substVarsHoles a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substVarsHoles (subst a (VarTy(id)) (HoleTy(id))) tail

let rec substHolesVars a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substHolesVars (subst a (HoleTy(id)) (VarTy(id))) tail

let rec extr(a:ty)(b:ty)(cont:ty*ty*map -> ty*ty*map):ty*ty*map = 
(*	print_type a; print_string ", "; print_type b; print_string "\n"; *)
	match a, b with
	| _, SkipTy -> cont (a, b, [])
	
	| _, HoleTy(_) -> raise (Fail("not defined"))

	| _, VarTy(_) -> raise (Fail("not defined"))

	| SkipTy, FunTy(_, _) -> cont (a, b, [])
	| HoleTy(_), FunTy(_, _) -> cont (a, b, [])
	| VarTy(id), FunTy(_, _) -> raise (Fail("failed at extract("^(string_of_int id)^", fun)"))	
	| FunTy(x, y), FunTy(x', y') when x=x' && y=y' -> extrAtomAtom a cont (* TODO: x'<:x /\ y<:y' *)
	| FunTy(x, y), FunTy(x', y') -> raise (Fail("failed at extract(fun, fun)"))
	| BasicTy(id), FunTy(_,_) -> raise (Fail("failed at extract("^id^", fun)"))
	| SeqTy(a1, a2), FunTy(_, _) -> extrSeqAtom a1 a2 b cont
	| ParTy(a1, a2), FunTy(_, _) -> extrParAtom a1 a2 b cont

	| SkipTy, BasicTy(_) -> cont (a, b, [])
	| HoleTy(_), BasicTy(_) -> cont (a, b, [])
	| VarTy(id), BasicTy(id') -> raise (Fail("failed at extract("^(string_of_int id)^", "^id'^")"))
	| FunTy(_,_), BasicTy(id) -> raise (Fail("failed at extract(fun, "^id^")"))
	| BasicTy(id), BasicTy(id') when id=id' -> extrAtomAtom a cont
	| BasicTy(id), BasicTy(id') -> raise (Fail("failed at extract("^id^", "^id'^")"))
	| SeqTy(a1, a2), BasicTy(_) -> extrSeqAtom a1 a2 b cont
	| ParTy(a1, a2), BasicTy(_) -> extrParAtom a1 a2 b cont

	| _, SeqTy(b1,b2) -> extrSeq a b1 b2 cont
	
	| _, ParTy(b1,b2) -> extrPar a b1 b2 cont

and extrAtomAtom a cont =
	let newId = freshId () in 
		cont (VarTy(newId), VarTy(newId), [(newId, a)])

and extrSeqAtom a1 a2 b cont =
	extr a1 b 
	(
		fun (a1', b', h1) -> 
			match b' with
			| VarTy(_) -> cont (SeqTy(a1', a2), b', h1)
			| _ when b'=b -> extr a2 b (fun (a2', b', h2) -> cont (SeqTy(a1',a2'), b', h2))
			| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
	)

and extrParAtom a1 a2 b cont =
	if inFst b a2 then
		Stack.push 
		(
			fun () ->
				extr a2 b 
				(
					fun (a2', b', h2) ->
						match b' with
						| VarTy(_) -> cont (ParTy(a1,a2'), b', h2)
						| _ when b'=b -> raise (Fail("error in extrParAtom"))
						| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
				)
		)
		!s
	;

	extr a1 b 
	(
		fun (a1', b', h1) ->
			match b' with
			| VarTy(_) ->	cont (ParTy(a1', a2), b', h1)
			| _ when b'=b -> 
				extr a2 b 
				(
					fun (a2', b', h2) ->
						match b' with
						| VarTy(_) -> raise (Fail("error in extrParAtom")) (* cont (ParTy(a1,a2'), SkipTy, h2) *)
						| _ when b'=b -> cont (ParTy(a1',a2'), b, [])
						| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
				)
			| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
	)

and extrSeq a b1 b2 cont =
	extr a b1 
	(
		fun (a', b1', h1) -> 
			if consistsOfVars b1' then
				let ah' = substVarsHoles a' h1 in
					extr ah' b2 
					(
						fun (ah'', b2', h2) ->
							let a'' = substHolesVars ah'' h1 in
								cont (a'', SeqTy(b1', b2'), h1@h2)
					)
			else
				cont (a', SeqTy(b1', b2), h1)
	)

and extrPar a b1 b2 cont =
	extr a b1 
	(
		fun (a', b1', h1) -> 
			extr a' b2 (fun (a'', b2', h2) -> cont (a'', ParTy(b1', b2'), h1@h2))
	)
;;

let rec init(a:ty)(b:ty):unit = 
	resetCount ();
	s := Stack.create (); 
	Stack.push ( fun () -> extr a b (fun (a', b', h) -> (a',b', h)) ) !s

let rec hasNext () = not (Stack.is_empty !s)

let rec next () = (Stack.pop !s) ()
