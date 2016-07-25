open Exceptions;;
open Types;;
open Assertions;;
open Exp;;
open Extract;;

let rec typecheck (a:assertion)(e:exp)(t:ty)(cont:assertion->unit):unit =
	match e, t with
	| Id(id), _ -> 
		Extract_a.extr a (Basic(id, t)) 
		(
			fun (a', b', h) -> 
				if consistsOfVars b' h then
					cont a'
				else
					raise (Fail("typechecking of the identifier"^(Hashtbl.find Lexer.tableIntStr id)^": not empty residue"))
		)
	| Select(e1, id), SkipTy -> typecheck a e1 (BasicTy(id)) cont
	| Select(_, _), _ -> raise (Fail("cannot extract a select expression with a type different than stop"))
	| Fun(x, tArg, tRet, e1), FunTy(argTy, retTy) ->
		let y = Lexer.freshId x in
			let e1' = Exp.substId e1 x y in
				typecheck (Par(a, Basic(y, tArg))) e1' tRet cont
	| Fun(_, _, _, _), _ -> raise (Fail("not a function type"))
(*	| Call(e1, e2), _ ->
		let refArgType = ref None in
			typecheck a e1 (FunTy(SomeTy(refArgType), t))
			(
				fun a' ->
					match !refArgType with
					| None -> raise (Fail("matching of argument failed"))
					| Some(t') -> typecheck a' e2 t' (fun a'' -> cont ()
			)
	| Let(id, t, e1, e2) ->
		typecheck a e1 t
		(
			fun a' -> 
		)
*)

let rec init(a:assertion)(e:exp)(t:ty)(cont:assertion->unit):unit = 
	Extract.resetCount ();
	Stack.push ( fun () -> typecheck a e t cont ) Extract.s

let rec hasNext () = not (Stack.is_empty Extract.s)

let rec next () = (Stack.pop Extract.s) ()
