open Types;;

exception Fail of string;;

let s:((unit -> ty*ty*map) Stack.t) ref = ref (Stack.create ());;

let rec extr(a:ty)(b:ty)(cont:ty*ty*map -> ty*ty*map):ty*ty*map = 
(*	print_type a; print_string ", "; print_type b; print_string "\n"; *)
	match a, b with
	| _, SkipTy -> cont (a, b, [])
	
	| _, Hole(_) -> raise (Fail("not defined"))

	| SkipTy, BasicTy(_) -> cont (a, b, [])
	| Hole(_), BasicTy(_) -> cont (a, b, [])
	| BasicTy(id), BasicTy(id') when id=id' -> extrAtomAtom a cont
	| BasicTy(id), BasicTy(id') -> raise (Fail("failed at extract("^id^", "^id'^")"))
	| Var(id), BasicTy(id') -> raise (Fail("failed at extract("^(string_of_int id)^", "^id'^")"))
	| SeqTy(a1,a2), BasicTy(_) -> extrSeqAtom a1 a2 b cont
	| ParTy(a1,a2), BasicTy(_) -> extrParAtom a1 a2 b cont
	
	| _, Var(_) -> raise (Fail("not defined"))

	| _, SeqTy(b1,b2) -> extrSeq a b1 b2 cont
	
	| _, ParTy(b1,b2) -> extrPar a b1 b2 cont

and extrAtomAtom a cont =
	let newId = freshId () in 
		cont (Var(newId), Var(newId), [(newId, a)])

and extrSeqAtom a1 a2 b cont =
	extr a1 b 
	(
		fun (a1', b', h1) -> 
			match b' with
			| Var(_) -> cont (SeqTy(a1', a2), b', h1)
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
						| Var(_) -> cont (ParTy(a1,a2'), b', h2)
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
			| Var(_) ->	cont (ParTy(a1', a2), b', h1)
			| _ when b'=b -> 
				extr a2 b 
				(
					fun (a2', b', h2) ->
						match b' with
						| Var(_) -> raise (Fail("error in extrParAtom")) (* cont (ParTy(a1,a2'), SkipTy, h2) *)
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
