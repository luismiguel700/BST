open Exceptions;;
open Types;;
open Assertions;;
open Unparser;;
open List;;

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
	match a, b with
	| _, Skip -> cont (a, b, [])
	
	| _, Hole(_) -> raise (Fail("unexpected extract type"))

	| _, Var(_) -> raise (Fail("unexpected extract type"))

	| Skip, Basic(_, _) -> cont (a, b, [])

	| Hole(_), Basic(_, _) ->  cont (a, b, [])  (* problem b = 0->0 ==> b consistsofvars .. but no! *)

	| Var(id), Basic(id', t) -> 
                if Types.isSkip t then cont (a, b, [])
                else raise (Fail("cannot extract "^(Hashtbl.find Lexer.tableIntStr id')^" fm Var("^(string_of_int id)^")"))

	| Basic(id, t), Basic(id', t') when id=id' -> 
		if Types.isSkip t then
		   cont (a, b, [])
		else if Types.isSkip t' then 
		   cont (a, b, [])
		else
		extrBasicBasic id t t' cont

	| Basic(id, t), Basic(id', t') -> 
		if Types.isSkip t then
		  cont (a, b, [])
		else if Types.isSkip t' then
		  cont (a, b, [])
		else
		  raise (Fail("cannot extract "^(Hashtbl.find Lexer.tableIntStr id')^" fm "^(Hashtbl.find Lexer.tableIntStr id)))

	| Seq(a1, a2), Basic(_, _) -> extrSeqAtom a1 a2 b cont

	| Par(a1, a2), Basic(id, _) -> extrParAtom a1 a2 b id cont

	| _, Seq(b1,b2) -> extrSeq a b1 b2 cont
	
	| _, Par(b1,b2) -> extrPar a b1 b2 cont

and extrBasicBasic id t1 t2 cont = 
	Extract.extr t1 t2
	(
		fun (t1', t2', h) ->
			if Types.consistsOfVarsEnv t1' h && Types.consistsOfVarsEnv t2' h then
				let newId = Extract.freshId () in
					cont (Var(newId), Var(newId), [(newId, Basic(id, t1))])
			else
				raise (Fail("cannot extract type fm "^(Hashtbl.find Lexer.tableIntStr id)))
	)	

and extrSeqAtom a1 a2 b cont =
        (* EXTR: Seq(a1,a2) b *)
	extr a1 b
	(
		fun (a1', b', h1) -> 
		    if Assertions.consistsOfVarsEnv b' h1 then
			cont ((mkSeq a1' a2), b', h1)
		    else 
		      extr a2 b ( fun (a2', b', h2) -> cont ( (mkSeq a1' a2'), b', h2) )
	)

and extrParAtom a1 a2 b id cont =
        (* EXTR: Par(a1,a2) b *)
        if inFst_act id a2 then
	Stack.push 
	(
		Stack.create (),
		fun () ->
			extr a2 b
			(
				fun (a2', b', h2) ->
				      if consistsOfVarsEnv b' h2 then cont ((mkPar a1 a2'), b', h2)
				      else raise (Fail("failed par branch -- backtrack"))
			)
	)
	Extract.stack;

	extr a1 b
	(
		fun (a1', b', h1) ->
		     if consistsOfVarsEnv b' h1 then cont ((mkPar a1' a2), b', h1)
                     else raise (Fail("failed par branch -- backtrack"))
	)

and extrSeq a b1 b2 cont =
	extr a b1
	(
		fun (a', b1', h1) -> 
			if Assertions.consistsOfVarsEnv b1' h1 then
				let ah' = substVarsHoles a' h1 in
					extr ah' b2
					(
						fun (ah'', b2', h2) ->
							let a'' = substHolesVars ah'' h1 in
								cont (a'', (mkSeq b1' b2'), h1@h2)
					)
			else if Assertions.consistsOfVarsEnv a' h1 then
				cont (a', (mkSeq b1' b2), h1)
			else
				raise (Fail("unexpected state"))
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
