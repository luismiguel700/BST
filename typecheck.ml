open Exceptions;;
open Types;;
open Assertions;;
open Exp;;
open Extract;;
open Extract_a;;
open Join;;
open List;;

let rec typecheck (a:assertion)(e:exp)(t:ty)(cont:(assertion*Extract_a.map)->unit):unit =
	match e, t with
	| Id(id), _ -> 
		Extract_a.extr a (Basic(id, t)) []
		(
			fun (a', b', h) -> 
				if consistsOfVars b' h then
					cont (a', h)
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
	| Call(e1, e2), _ ->
		let refArgType = ref None in
			typecheck a e1 (FunTy(SomeTy(refArgType), t))
			(
				fun (a', h1) ->
					match !refArgType with
					| None -> raise (Fail("matching of argument failed"))
					| Some(tArg) -> 
						typecheck a' e2 tArg
						(
							fun (a'', h2) -> 
								let newId:int = freshId () in
									let (b, (_, _)) = join_a (map (fun (id, _) -> id) (h1@h2)) a'' (h1@h2) newId in
										cont (b, [(newId, Skip)]) (* corrigir *)
						)
			)
	| Let(id, tE1, e1, e2), _ ->
		typecheck a e1 tE1
		(
			fun (a', h1) ->
				 let newId = freshId () in
					let (b, (_, _)) = join_a (map (fun (id, _) -> id) h1) a' h1 newId in
						typecheck (subst b (Var(newId)) (Basic(id, tE1))) e2 t cont
		)


let rec init(a:assertion)(e:exp)(t:ty)(cont:(assertion*Extract_a.map)->unit):unit = 
	Extract.resetCount ();
	Stack.push ( Stack.create(), fun () -> typecheck a e t cont ) Extract.s

let rec hasNext () = not (Stack.is_empty Extract.s)

let rec next () = 
	let (stackRefs, cont) = Stack.pop Extract.s in
	(
		while not (Stack.is_empty stackRefs) do
			(Stack.pop stackRefs) := None
		done;
		cont ()
	)
