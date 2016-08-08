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
	| Seq(a1,a2) -> mkSeq (deleteId a1 id) (deleteId a2 id)
	| Par(a1,a2) -> mkPar (deleteId a1 id) (deleteId a2 id)
	| Basic(id', t) when id=id' -> 
		if Types.isSkip t then
			Skip
		else
			raise (Fail("id"^(Hashtbl.find Lexer.tableIntStr id)^" linear type at end of scope"))
	| Basic(_, _) -> a
	| _ -> a

let rec typecheck (a:assertion)(e:exp)(t:ty)(cont:(assertion*Extract_a.map)->unit):unit =
	match e, t with
	| Id(id), _ -> typecheckId a id t cont
	| Select(e1, id), SkipTy -> typecheck a e1 (BasicTy(id)) cont
	| Select(_, _), _ -> raise (Fail("label type is not stop")) 
	| Fun(id, tArg, tRet, e1), FunTy(tArg2, tRet2) -> (* LC: corrected confusion in alpha conversion *)
		let id' = Lexer.freshId id in
		let e1' = Exp.substId e1 id id' in
		typecheckFun a id' tArg tRet e1' tArg2 tRet2 cont
	| Fun(_, _, _, _), _ -> raise (Fail("function type expected."))
	| Call(e1, e2), _ -> typecheckCall a e1 e2 t cont
	| Let(id, tE1, e1, e2), _ ->  (* corrected confusion in alpha conversion *)
		let id' = Lexer.freshId id in
		let e2' = Exp.substId e2 id id' in
		typecheckLet a id' tE1 e1 e2' t cont
	| Seqe(e1, e2), _ ->  
		let dmy = Lexer.freshId 0 in (* dmy id don't care, write a proper typecheckSeqe *)
		typecheckLet a dmy SkipTy e1 e2 t cont


and typecheckId a id t cont =
        let as1 = (makeAssertion id t) in
	Extract_a.extr a as1
	(
		fun (a', b', h) -> (
			if consistsOfVars b' (map (fst) h) then (
				cont (a', h) )
			else
				raise (Fail("cannot extract type for "^(Hashtbl.find Lexer.tableIntStr id)))
                 )
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
				| None -> raise (Fail("unexpected state")) 
				| Some(tArg) -> 
					typecheck a' e2 tArg
					(
						fun (a'', h2) -> 
							let newId = freshId () in
                                                        let h12 = h1@h2 in
                                                        let b =  if (h12 = []) then mkPar (Var(newId)) a''
                                                                 else join_as (map (fun (id, _) -> id) h12) a'' newId
                                                                 in cont (b, [(newId, Skip)])
					)
		)
			
and typecheckLet a id tE1 e1 e2 t cont =
	typecheck a e1 tE1
	(
		fun (a', h1) -> 
			let newId = freshId () in
                                let ac = if (h1 = []) then mkPar (Var(newId)) a'
                                                      else join_as (map (fun (id, _) -> id) h1) a' newId
                                in (
						typecheck  (subst ac (Var(newId)) (makeAssertion id tE1)) e2 t
						(
							fun (a'', h2) -> 
							 cont (deleteId a'' id, h2)
						))
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
