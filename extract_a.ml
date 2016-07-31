open Exceptions;;
open Types;;
open Assertions;;
open Unparser;;

type map = (int * assertion) list (* optimizar mais tarde com hashmaps *)

let rec substVarsHoles a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substVarsHoles (subst a (Var(id)) (Hole(id))) tail

let rec substHolesVars a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substHolesVars (subst a (Hole(id)) (Var(id))) tail

let rec extr(a:assertion)(b:assertion)(cont:(assertion*assertion*map)->unit):unit = 
(*	print_assertion a; print_string ", "; print_assertion b; print_string "\n"; *)
	match a, b with
	| _, Skip -> cont (a, b, [])
	
	| _, Hole(_) -> raise (Fail("not defined"))

	| _, Var(_) -> raise (Fail("not defined"))

	| Skip, Basic(_, _) -> cont (a, b, [])

	| Hole(_), Basic(_, _) -> cont (a, b, [])

	| Var(id), Basic(id', _) -> raise (Fail("failed at extract("^(string_of_int id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))

	| Basic(id, t), Basic(id', t') when id=id' -> 
		if Types.isSkip t || Types.isSkip t' then 
			cont (a, b, [])
		else
			extrBasicBasic id t t' cont (* rever *)

	| Basic(id, t), Basic(id', t') -> 
		if Types.isSkip t || Types.isSkip t' then 
			cont (a, b, [])
		else
			raise (Fail("failed at extract("^(Hashtbl.find Lexer.tableIntStr id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))

	| Seq(a1, a2), Basic(_, _) -> extrSeqAtom a1 a2 b cont

	| Par(a1, a2), Basic(_, _) -> extrParAtom a1 a2 b cont

	| _, Seq(b1,b2) -> extrSeq a b1 b2 cont
	
	| _, Par(b1,b2) -> extrPar a b1 b2 cont

and extrBasicBasic id t1 t2 cont =
	Extract.extr t1 t2
	(
		fun (t1', t2', h) ->
			if Types.consistsOfVars t1' h && Types.consistsOfVars t2' h then
				let newId = Extract.freshId () in
					cont (Var(newId), Var(newId), [(newId, Basic(id, t1))])
			else
				raise (Fail("failed at extracting id "^(Hashtbl.find Lexer.tableIntStr id)))
	)	

and extrSeqAtom a1 a2 b cont =
	extr a1 b
	(
		fun (a1', b', h1) -> 
			match b' with
			| Var(_) -> cont (Seq(a1', a2), b', h1)
			| _ when b'=b -> 
					extr a2 b 
					(
						fun (a2', b', h2) -> 
							cont ((if isSkip a1' then a2' else Seq(a1', a2')), b', h2)
					)
			| _ -> raise (Fail("the residue should be a var or the atom being extracted"))
	)

and extrParAtom a1 a2 b cont =
	Stack.push 
	(
		Stack.create (),
		fun () ->
			extr a2 b
			(
				fun (a2', b', h2) ->
					match b' with
					| Var(_) -> cont ((mkPar a1 a2'), b', h2)
					| _ when b'=b -> raise (Fail("error in extrParAtom"))
					| _ -> raise (Fail("the residue should be a var or the atom being extracted"))
			)
	)
	Extract.stack;

	extr a1 b
	(
		fun (a1', b', h1) ->
			match b' with
			| Var(_) ->	cont ((mkPar a1' a2), b', h1)
			| _ when b'=b -> 
				extr a2 b
				(
					fun (a2', b', h2) ->
						match b' with
						| Var(_) -> raise (Fail("error in extrParAtom")) (* cont (Par(a1,a2'), SkipTy, h2) *)
						| _ when b'=b -> cont ((mkPar a1' a2'), b, [])
						| _ -> raise (Fail("the residue should be a var or the atom being extracted"))
				)
			| _ -> raise (Fail("the residue should be a var or the atom being extracted"))
	)

and extrSeq a b1 b2 cont =
	extr a b1
	(
		fun (a', b1', h1) -> 
			if consistsOfVars b1' h1 then
				let ah' = substVarsHoles a' h1 in
					extr ah' b2
					(
						fun (ah'', b2', h2) ->
							let a'' = substHolesVars ah'' h1 in
								cont (a'', Seq(b1', b2'), h1@h2)
					)
			else if consistsOfVars a' h1 then
				cont (a', Seq(b1', b2), h1)
			else
				raise (Fail("one of the residues should only contain vars"))
	)

and extrPar a b1 b2 cont =
	extr a b1
	(
		fun (a', b1', h1) -> 
			extr a' b2 (fun (a'', b2', h2) -> cont (a'', mkPar b1' b2', h1@h2))
	)
;;

let rec init(a:assertion)(b:assertion)(cont:(assertion*assertion*map)->unit):unit = 
	Extract.resetCount ();
	let a' = makeCanonical a in
	let b' = makeCanonical b in
		Stack.push ( Stack.create (), fun () -> extr a' b' cont ) Extract.stack

let rec hasNext () = not (Stack.is_empty Extract.stack)

let rec next () = 
	let (stackRefs, cont) = Stack.pop Extract.stack in
	(
		while not (Stack.is_empty stackRefs) do
			(Stack.pop stackRefs) := None
		done;
		cont ()
	)
