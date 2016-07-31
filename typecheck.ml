open Exceptions;;
open Types;;
open Assertions;;
open Exp;;
open Extract;;
open Extract_a;;
open Join;;
open List;;
open Unparser;;

(* deletes all occurrences of id:T from A *)
(* throws an exception if: not T<:>0 *)
let rec deleteId a id =
	match a with
	| Skip -> a
	| Hole(_) -> a
	| Var(_) -> a
	| Basic(id', t) when id=id' -> 
		if Types.isSkip t then
			Skip
		else
			raise (Fail("cannot delete id"^(Hashtbl.find Lexer.tableIntStr id)^" its type is not stop"))
	| Basic(_, _) -> a
	| Seq(a1,a2) -> mkSeq (deleteId a1 id) (deleteId a2 id)
	| Par(a1,a2) -> mkPar (deleteId a1 id) (deleteId a2 id)

(*

what are the assumptions in the arguments of typecheck ?
Is the assertion "a" "variable/hole" free ?
Does the assertion passed to the continuation always have one or zero variables ? 
 
*)

let rec typecheck (a:assertion)(e:exp)(t:ty)(cont:(assertion*Extract_a.map)->unit):unit =
	print_string "tcheck "; print_assertion a; print_string "\t"; print_exp e; print_string "\t"; print_type t; print_string "\n"; 
	match e, t with
	| Id(id), _ -> typecheckId a id t cont
	| Select(e1, id), SkipTy -> typecheck a e1 (BasicTy(id)) cont
	| Select(_, _), _ -> raise (Fail("cannot extract a select expression with a type different from stop"))
	| Fun(id, tArg, tRet, e1), FunTy(tArg2, tRet2) -> (* corrected confusion in alpha conversion *)
		let id' = Lexer.freshId id in
		let e1' = Exp.substId e1 id id' in
		typecheckFun a id' tArg tRet e1' tArg2 tRet2 cont
	| Fun(_, _, _, _), _ -> raise (Fail("not a function type"))
	| Call(e1, e2), _ -> typecheckCall a e1 e2 t cont
	| Let(id, tE1, e1, e2), _ ->  (* corrected confusion in alpha conversion *)
		let id' = Lexer.freshId id in
		let e2' = Exp.substId e2 id id' in
		typecheckLet a id' tE1 e1 e2' t cont

and typecheckId a id t cont =
	print_string "tcheck_id "; print_assertion a; print_string " "; print_int id; print_string ":"; print_type t; print_string "\n"; 

	Extract_a.extr a (makeAssertion id t)
	(
		fun (a', b', h) -> 
			if consistsOfVars b' h then (
                                (* shouldn't we JOIN? what is the invariant ?? *)
				cont (a', h) )
			else
				raise (Fail("typechecking of the identifier"^(Hashtbl.find Lexer.tableIntStr id)^": not empty residue"))
	)

and typecheckFun a id tArg tRet e1 tArg2 tRet2 cont =
	Extract.extr (FunTy(tArg, tRet)) (FunTy(tArg2, tRet2))
	(
		fun (_, _, _) ->
					typecheck (mkPar a (makeAssertion id tArg)) e1 tRet
					(
						fun (a', h) -> 
						let clean = deleteId a' id in cont (clean, h)
					)
	)

and typecheckCall a e1 e2 t cont = 
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
							let newId = freshId () in
								let b = join_a (map (fun (id, _) -> id) (h1@h2)) a'' (h1@h2) newId in
									cont (b, [(newId, Skip)])
					)
		)
			
and typecheckLet a id tE1 e1 e2 t cont =
	typecheck a e1 tE1
	(
		fun (a', h1) -> 
			let newId = freshId () in
				print_assertion a'; print_string " :-|\n"; 
				let b = join_a (map (fun (id, _) -> id) h1) a' h1 newId in
						typecheck (subst b (Var(newId)) (Basic(id, tE1))) e2 t
						(
							fun (a'', h2) -> 
							 (  	print_assertion a''; print_string " body!\n" ;
								cont (deleteId a'' id, h2) )
						)
	)

let rec init(a:assertion)(e:exp)(t:ty)(cont:(assertion*Extract_a.map)->unit):unit = 
	Extract.resetCount ();
	let a' = makeCanonical a in
		Stack.push ( Stack.create(), fun () -> typecheck a' e t cont ) Extract.stack

let rec hasNext () = not (Stack.is_empty Extract.stack)

let rec next () = 
	let (stackRefs, cont) = Stack.pop Extract.stack in
	(
		while not (Stack.is_empty stackRefs) do
			(Stack.pop stackRefs) := None
		done;
		cont ()
	)
