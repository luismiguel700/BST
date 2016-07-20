open Types;;
open Unparser;;

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
	| SomeTy(_), _ -> raise (Fail("not defined"))

	| _, SkipTy -> cont (a, b, [])
	
	| _, HoleTy(_) -> raise (Fail("not defined"))

	| _, VarTy(_) -> raise (Fail("not defined"))

	| _, SomeTy(_) -> raise (Fail("not defined"))

	| SkipTy, FunTy(_, _) -> cont (a, b, [])
	| HoleTy(_), FunTy(_, _) -> cont (a, b, [])
	| VarTy(id), FunTy(_, _) -> raise (Fail("failed at extract("^(string_of_int id)^", fun)"))	
	| FunTy(x, y), FunTy(x', y') -> extrFunFun x y x' y' cont
	| BasicTy(id), FunTy(_,_) -> raise (Fail("failed at extract("^(Hashtbl.find Lexer.tableIntStr id)^", fun)"))
	| SeqTy(a1, a2), FunTy(_, _) -> extrSeqAtom a1 a2 b cont
	| ParTy(a1, a2), FunTy(_, _) -> extrParAtom a1 a2 b cont

	| SkipTy, BasicTy(_) -> cont (a, b, [])
	| HoleTy(_), BasicTy(_) -> cont (a, b, [])
	| VarTy(id), BasicTy(id') -> raise (Fail("failed at extract("^(string_of_int id)^", "^(Hashtbl.find Lexer.tableIntStr id)^")"))
	| FunTy(_,_), BasicTy(id) -> raise (Fail("failed at extract(fun, "^(Hashtbl.find Lexer.tableIntStr id)^")"))
	| BasicTy(id), BasicTy(id') when id=id' -> extrAtomAtom a cont
	| BasicTy(id), BasicTy(id') -> raise (Fail("failed at extract("^(Hashtbl.find Lexer.tableIntStr id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))
	| SeqTy(a1, a2), BasicTy(_) -> extrSeqAtom a1 a2 b cont
	| ParTy(a1, a2), BasicTy(_) -> extrParAtom a1 a2 b cont

	| _, SeqTy(b1,b2) -> extrSeq a b1 b2 cont
	
	| _, ParTy(b1,b2) -> extrPar a b1 b2 cont

and extrFunFun arg1 ret1 arg2 ret2 cont =
	extr ret1 ret2
	(
		fun (ret1', ret2', h1) ->
			if consistsOfVars ret1' h1 && consistsOfVars ret2' h1 then
				match arg2 with
				| SomeTy(pointer) -> 
					(	
						try
							pointer := Some(arg1);
							let newId = freshId () in 
								cont (FunTy(VarTy(newId), ret1'), FunTy(VarTy(newId), ret2'), (newId, arg1)::h1)
						with
						| Fail(s) -> pointer := None; raise (Fail(s))
					)
				| _ -> 
					extr arg2 arg1 
					(		
						fun (arg1', arg2', h2) ->	
							if consistsOfVars arg1' h2 && consistsOfVars arg2' h2 then
								cont (FunTy(arg1', ret1'), FunTy(arg2', ret2'), h1@h2)
							else
								raise (Fail("failed while extracting fun type")) 
					)
			else
				raise (Fail("failed while extracting fun type")) 
	)

and extrAtomAtom a cont =
	let newId = freshId () in 
		cont (VarTy(newId), VarTy(newId), [(newId, a)])

and extrSeqAtom a1 a2 b cont =
	extr a1 b 
	(
		fun (a1', b', h1) -> 
			if consistsOfVars b' h1 then
				cont (SeqTy(a1', a2), b', h1)
			else if b'=b then
				extr a2 b (fun (a2', b', h2) -> cont (SeqTy(a1',a2'), b', h2))
			else
				raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
	)

and extrParAtom a1 a2 b cont =
	if inFst b a2 then
		Stack.push 
		(
			fun () ->
				extr a2 b 
				(
					fun (a2', b', h2) ->
						if consistsOfVars b' h2 then
							cont (ParTy(a1,a2'), b', h2)
						else if b'=b then
							raise (Fail("error in extrParAtom"))
						else
							raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
				)
		)
		!s
	;

	extr a1 b 
	(
		fun (a1', b', h1) ->
			if consistsOfVars b' h1 then
				cont (ParTy(a1', a2), b', h1)
			else if b'=b then
				extr a2 b 
				(
					fun (a2', b', h2) ->
						if consistsOfVars b' h2 then
							raise (Fail("error in extrParAtom")) (* cont (ParTy(a1,a2'), SkipTy, h2) *)
						else if b'=b then
							cont (ParTy(a1',a2'), b, [])
						else
							raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
				)
			else
				raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
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
