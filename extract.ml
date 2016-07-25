open Exceptions;;
open Types;;
open Unparser;;
open List;;

type map = (int * ty) list (* optimizar mais tarde com hashmaps *)

let s:( ((some ref) Stack.t) * (unit -> unit) ) Stack.t = Stack.create ();;

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

let rec extr(a:ty)(b:ty)(stacksOfRefs:((some ref) Stack.t) list)(cont:ty*ty*map -> unit):unit = 
	print_type a; print_string ", "; print_type b; print_string "\n";
	match a, b with
	| SomeTy(_), _ -> raise (Fail("not defined"))

	| _, SkipTy -> cont (a, b, [])
	
	| _, HoleTy(_) -> raise (Fail("not defined"))

	| _, VarTy(_) -> raise (Fail("not defined"))

	| _, SomeTy(_) -> raise (Fail("not defined"))

	| SkipTy, FunTy(_, _) -> cont (a, b, [])
	| HoleTy(_), FunTy(_, _) -> cont (a, b, [])
	| VarTy(id), FunTy(_, _) -> raise (Fail("failed at extract("^(string_of_int id)^", fun)"))	
	| FunTy(x, y), FunTy(x', y') -> extrFunFun x y x' y' stacksOfRefs cont
	| BasicTy(id), FunTy(_,_) -> raise (Fail("failed at extract("^(Hashtbl.find Lexer.tableIntStr id)^", fun)"))
	| SeqTy(a1, a2), FunTy(_, _) -> extrSeqAtom a1 a2 b stacksOfRefs cont
	| ParTy(a1, a2), FunTy(_, _) -> extrParAtom a1 a2 b stacksOfRefs cont

	| SkipTy, BasicTy(_) -> cont (a, b, [])
	| HoleTy(_), BasicTy(_) -> cont (a, b, [])
	| VarTy(id), BasicTy(id') -> raise (Fail("failed at extract("^(string_of_int id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))
	| FunTy(_,_), BasicTy(id) -> raise (Fail("failed at extract(fun, "^(Hashtbl.find Lexer.tableIntStr id)^")"))
	| BasicTy(id), BasicTy(id') when id=id' -> extrAtomAtom a cont
	| BasicTy(id), BasicTy(id') -> raise (Fail("failed at extract("^(Hashtbl.find Lexer.tableIntStr id)^", "^(Hashtbl.find Lexer.tableIntStr id')^")"))
	| SeqTy(a1, a2), BasicTy(_) -> extrSeqAtom a1 a2 b stacksOfRefs cont
	| ParTy(a1, a2), BasicTy(_) -> extrParAtom a1 a2 b stacksOfRefs cont

	| _, SeqTy(b1,b2) -> extrSeq a b1 b2 stacksOfRefs cont
	
	| _, ParTy(b1,b2) -> extrPar a b1 b2 stacksOfRefs cont

and extrFunFunWithSome arg1 ret1' arg2Ptr ret2' hRet stacksOfRefs cont =
	match !arg2Ptr with
	| None ->
		arg2Ptr := Some(arg1);
		iter (fun stack -> Stack.push arg2Ptr stack) stacksOfRefs;
			let newId = freshId () in 
				cont (FunTy(VarTy(newId), ret1'), FunTy(VarTy(newId), ret2'), (newId, arg1)::hRet)
	| Some(t) -> 
		extr t arg1 stacksOfRefs
		(		
			fun (arg1', arg2', hArg) ->	
				if consistsOfVars arg1' hArg && consistsOfVars arg2' hArg then
					cont (FunTy(arg1', ret1'), FunTy(arg2', ret2'), hRet@hArg)
				else
					raise (Fail("failed while extracting fun type")) 
		)

and extrFunFun arg1 ret1 arg2 ret2 stacksOfRefs cont =
	extr ret1 ret2 stacksOfRefs
	(
		fun (ret1', ret2', hRet) ->
			if consistsOfVars ret1' hRet && consistsOfVars ret2' hRet then
				match arg2 with
				| SomeTy(pointer) -> extrFunFunWithSome arg1 ret1' pointer ret2' hRet stacksOfRefs cont
				| _ -> 
					extr arg2 arg1 stacksOfRefs
					(		
						fun (arg1', arg2', hArg) ->	
							if consistsOfVars arg1' hArg && consistsOfVars arg2' hArg then
								cont (FunTy(arg1', ret1'), FunTy(arg2', ret2'), hRet@hArg)
							else
								raise (Fail("failed while extracting fun type")) 
					)
			else
				raise (Fail("failed while extracting fun type")) 
	)

and extrAtomAtom a cont =
	let newId = freshId () in 
		cont (VarTy(newId), VarTy(newId), [(newId, a)])

and extrSeqAtom a1 a2 b stacksOfRefs cont =
	extr a1 b stacksOfRefs 
	(
		fun (a1', b', h1) -> 
			if consistsOfVars b' h1 then
				cont (SeqTy(a1', a2), b', h1)
			else if b'=b then
				extr a2 b stacksOfRefs (fun (a2', b', h2) -> cont (SeqTy(a1',a2'), b', h2))
			else
				raise (Fail("the residue should be equal to 0 or to the atom being extracted"))
	)

and extrParAtom a1 a2 b stacksOfRefs cont =
	let stackRefs = Stack.create () in
		Stack.push 
		(
			stackRefs,
			fun () ->
				extr a2 b stacksOfRefs
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
		s
		;

		extr a1 b (stackRefs::stacksOfRefs)
		(
			fun (a1', b', h1) ->
				if consistsOfVars b' h1 then
					cont (ParTy(a1', a2), b', h1)
				else if b'=b then
					extr a2 b stacksOfRefs
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
								cont (a'', SeqTy(b1', b2'), h1@h2)
					)
			else
				cont (a', SeqTy(b1', b2), h1)
	)

and extrPar a b1 b2 stacksOfRefs cont =
	extr a b1 stacksOfRefs
	(
		fun (a', b1', h1) -> 
			extr a' b2 stacksOfRefs (fun (a'', b2', h2) -> cont (a'', ParTy(b1', b2'), h1@h2))
	)
;;

let rec init(a:ty)(b:ty)(cont:(ty*ty*map)->unit):unit = 
	resetCount ();
	Stack.push (Stack.create (), fun () -> extr a b [] cont) s

let rec hasNext () = not (Stack.is_empty s)

let rec next () = 
	let (stackRefs, cont) = Stack.pop s in
	(
		while not (Stack.is_empty stackRefs) do
			(Stack.pop stackRefs) := None
		done;
		cont ()
	)
