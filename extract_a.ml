open Assertions;;

type map = (int * assertion) list (* optimizar mais tarde com hashmaps *)
type res_extr_a = assertion*assertion*map;;

exception Fail of string;;

let s:((unit -> assertion*assertion*map) Stack.t) ref = ref (Stack.create ());;

let idCount = ref 0;;

let resetCount () = idCount := 0

let freshId () = (idCount := (!idCount+1)); !idCount

let rec substVarsHoles a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substVarsHoles (subst a (Var(id)) (Hole(id))) tail

let rec substHolesVars a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substHolesVars (subst a (Hole(id)) (Var(id))) tail

let rec extr(a:assertion)(b:assertion)(cont: res_extr_a->res_extr_a):res_extr_a = 
(*	print_type a; print_string ", "; print_type b; print_string "\n"; *)
	match a, b with
	| _, Skip -> cont (a, b, [])
	
	| _, Hole(_) -> raise (Fail("not defined"))

	| _, Var(_) -> raise (Fail("not defined"))

	| Skip, Basic(_, _) -> cont (a, b, [])
	| Hole(_), Basic(_, _) -> cont (a, b, [])
	| Var(id), Basic(id', _) -> raise (Fail("failed at extract("^(string_of_int id)^", "^id'^")"))
	| Basic(id, t), Basic(id', t') when id=id' && t=t' -> extrAtomAtom a cont (* rever *)
	| Basic(id, _), Basic(id', _) -> raise (Fail("failed at extract("^id^", "^id'^")"))
	| Seq(a1, a2), Basic(_, _) -> extrSeqAtom a1 a2 b cont
	| Par(a1, a2), Basic(_, _) -> extrParAtom a1 a2 b cont

	| _, Seq(b1,b2) -> extrSeq a b1 b2 cont
	
	| _, Par(b1,b2) -> extrPar a b1 b2 cont

and extrAtomAtom a cont =
	let newId = freshId () in 
		cont (Var(newId), Var(newId), [(newId, a)])

and extrSeqAtom a1 a2 b cont =
	extr a1 b 
	(
		fun (a1', b', h1) -> 
			match b' with
			| Var(_) -> cont (Seq(a1', a2), b', h1)
			| _ when b'=b -> extr a2 b (fun (a2', b', h2) -> cont (Seq(a1',a2'), b', h2))
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
						| Var(_) -> cont (Par(a1,a2'), b', h2)
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
			| Var(_) ->	cont (Par(a1', a2), b', h1)
			| _ when b'=b -> 
				extr a2 b 
				(
					fun (a2', b', h2) ->
						match b' with
						| Var(_) -> raise (Fail("error in extrParAtom")) (* cont (Par(a1,a2'), SkipTy, h2) *)
						| _ when b'=b -> cont (Par(a1',a2'), b, [])
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
								cont (a'', Seq(b1', b2'), h1@h2)
					)
			else
				cont (a', Seq(b1', b2), h1)
	)

and extrPar a b1 b2 cont =
	extr a b1 
	(
		fun (a', b1', h1) -> 
			extr a' b2 (fun (a'', b2', h2) -> cont (a'', Par(b1', b2'), h1@h2))
	)
;;

let rec init(a:assertion)(b:assertion):unit = 
	resetCount ();
	s := Stack.create (); 
	Stack.push ( fun () -> extr a b (fun (a', b', h) -> (a',b', h)) ) !s

let rec hasNext () = not (Stack.is_empty !s)

let rec next () = (Stack.pop !s) ()
