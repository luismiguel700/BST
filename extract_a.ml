open Exceptions;;
open Types;;
open Assertions;;

type map = (int * assertion) list (* optimizar mais tarde com hashmaps *)

let rec substVarsHoles a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substVarsHoles (subst a (Var(id)) (Hole(id))) tail

let rec substHolesVars a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substHolesVars (subst a (Hole(id)) (Var(id))) tail

let rec extr(a:assertion)(b:assertion)(stacksOfRefs:((some ref) Stack.t) list)(cont:(assertion*assertion*map)->unit):unit = 
(*	print_type a; print_string ", "; print_type b; print_string "\n"; *)
	match a, b with
	| _, Skip -> cont (a, b, [])
	
	| _, Hole(_) -> raise (Fail("not defined"))

	| _, Var(_) -> raise (Fail("not defined"))

	| Skip, Basic(_, _) -> cont (a, b, [])
	| Hole(_), Basic(_, _) -> cont (a, b, [])
	| Var(id), Basic(id', _) -> raise (Fail("failed at extract("^(string_of_int id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))
	| Basic(id, t), Basic(id', t') when id=id' -> extrBasicBasic id t t' stacksOfRefs cont (* rever *)
	| Basic(id, _), Basic(id', _) -> raise (Fail("failed at extract("^(Hashtbl.find Lexer.tableIntStr id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))
	| Seq(a1, a2), Basic(_, _) -> extrSeqAtom a1 a2 b stacksOfRefs cont
	| Par(a1, a2), Basic(_, _) -> extrParAtom a1 a2 b stacksOfRefs cont

	| _, Seq(b1,b2) -> extrSeq a b1 b2 stacksOfRefs cont
	
	| _, Par(b1,b2) -> extrPar a b1 b2 stacksOfRefs cont

and extrBasicBasic id t1 t2 stacksOfRefs cont =
	Extract.extr t1 t2 stacksOfRefs
	(
		fun (t1', t2', h) ->
			if Types.consistsOfVars t1' h && Types.consistsOfVars t2' h then
				let newId = Extract.freshId () in
					cont (Var(newId), Var(newId), [(newId, Basic(id, t1))])
			else
				raise (Fail("failed at extracting id "^(Hashtbl.find Lexer.tableIntStr id)))
	)	

and extrSeqAtom a1 a2 b stacksOfRefs cont =
	extr a1 b stacksOfRefs
	(
		fun (a1', b', h1) -> 
			match b' with
			| Var(_) -> cont (Seq(a1', a2), b', h1)
			| _ when b'=b -> extr a2 b stacksOfRefs (fun (a2', b', h2) -> cont (Seq(a1',a2'), b', h2))
			| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
	)

and extrParAtom a1 a2 b stacksOfRefs cont =
	let stackRefs = Stack.create () in
	(
		Stack.push 
		(
			stackRefs,
			fun () ->
				extr a2 b stacksOfRefs
				(
					fun (a2', b', h2) ->
						match b' with
						| Var(_) -> cont (Par(a1,a2'), b', h2)
						| _ when b'=b -> raise (Fail("error in extrParAtom"))
						| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
				)
		)
		Extract.s;

		extr a1 b (stackRefs::stacksOfRefs)
		(
			fun (a1', b', h1) ->
				match b' with
				| Var(_) ->	cont (Par(a1', a2), b', h1)
					| _ when b'=b -> 
					extr a2 b (stackRefs::stacksOfRefs)
					(
						fun (a2', b', h2) ->
							match b' with
							| Var(_) -> raise (Fail("error in extrParAtom")) (* cont (Par(a1,a2'), SkipTy, h2) *)
							| _ when b'=b -> cont (Par(a1',a2'), b, [])
							| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
					)
				| _ -> raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
		)
	)

and extrSeq a b1 b2 stacksOfRefs cont =
	extr a b1 stacksOfRefs
	(
		fun (a', b1', h1) -> 
			if consistsOfVars b1' h1 then
				let ah' = substVarsHoles a' h1 in
					extr ah' b2 stacksOfRefs
					(
						fun (ah'', b2', h2) ->
							let a'' = substHolesVars ah'' h1 in
								cont (a'', Seq(b1', b2'), h1@h2)
					)
			else
				cont (a', Seq(b1', b2), h1)
	)

and extrPar a b1 b2 stacksOfRefs cont =
	extr a b1 stacksOfRefs
	(
		fun (a', b1', h1) -> 
			extr a' b2 stacksOfRefs (fun (a'', b2', h2) -> cont (a'', Par(b1', b2'), h1@h2))
	)
;;

let rec init(a:assertion)(b:assertion)(cont:(assertion*assertion*map)->unit):unit = 
	Extract.resetCount ();
	Stack.push ( Stack.create (), fun () -> extr a b [] cont ) Extract.s

let rec hasNext () = not (Stack.is_empty Extract.s)

let rec next () = 
	let (stackRefs, cont) = Stack.pop Extract.s in
	(
		while not (Stack.is_empty stackRefs) do
			(Stack.pop stackRefs) := None
		done;
		cont ()
	)
