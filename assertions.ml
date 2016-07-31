open List;;
open Types;;
open Exceptions;;

type assertion =
| Skip
| Hole of int
| Var of int
| Basic of int * ty
| Seq of assertion * assertion
| Par of assertion * assertion
;;

let rec makeAssertion id t =
	match t with
	| SkipTy -> Basic(id, SkipTy)
	| HoleTy(id) -> raise (Fail("makeAssertion cannot be applied to holes"))
	| VarTy(id) -> raise (Fail("makeAssertion cannot be applied to var"))
	| SomeTy(_) -> raise (Fail("makeAssertion cannot be applied to SomeType"))
	| FunTy(_, _) -> Basic(id, t)
	| BasicTy(id') -> Basic(id, BasicTy(id'))
	| SeqTy(a1, a2) -> Seq(makeAssertion id a1, makeAssertion id a2)
	| ParTy(a1, a2) -> Par(makeAssertion id a1, makeAssertion id a2)

let rec makeCanonical a =
	match a with
	| Skip -> a
	| Hole(_) -> a
	| Var(_) -> a
	| Basic(id, t) -> makeAssertion id t
	| Seq(a1, a2) -> Seq(makeCanonical a1, makeCanonical a2)
	| Par(a1, a2) -> Par(makeCanonical a1, makeCanonical a2)

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| Skip -> true
	| Hole(_) -> false
	| Var(_) -> false
	| Basic(_, t) -> Types.isSkip t
	| Seq(a1,a2) -> if isSkip a1 then isSkip a2 else false
	| Par(a1,a2) -> if isSkip a1 then isSkip a2 else false

(* A=C[t] *)

let rec inFst t a = 
	match a with
	| Skip -> false
	| Seq(a1,a2) -> 
		if isSkip a1 then
			inFst t a2
		else
			inFst t a1
	| Par(a1,a2) -> if inFst t a1 then true else inFst t a2 
	| _ -> t=a
;;

(* A=A'[~x] ? *) (* optmimizar mais tarde *)
let rec containsVars a xs =
	match a with
	| Skip -> false
	| Hole(_) -> false
	| Var(id) -> mem id xs
	| Basic(_, _) -> false
	| Seq(a1,a2) -> containsVars a1 xs || containsVars a2 xs
	| Par(a1,a2) -> containsVars a1 xs || containsVars a2 xs

let rec consistsOfVars a vars =
	match a with
	| Skip -> true
	| Hole(_) -> true
	| Var(id) -> exists (fun (id', _) -> id=id') vars
	| Basic(_, t) -> Types.consistsOfVars t vars
	| Seq(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars
	| Par(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars

(* A{C/b} *)
let rec subst a b c =
	match a with
	| Skip -> a
	| Hole(_) -> if a=b then c else a
	| Var(_) -> if a=b then c else a
	| Basic(_, _) -> if a=b then c else a
	| Seq(a1,a2) -> Seq(subst a1 b c, subst a2 b c)
	| Par(a1,a2) -> Par(subst a1 b c, subst a2 b c)

let mkPar t1 t2 = 
	match t1, t2 with
	| Skip, _ -> t2
	| _, Skip -> t1
	| Basic(_,ty1), _ -> (
            if Types.isSkip ty1 then t2 else 
                match t2 with
                  Basic(_,ty2) -> if Types.isSkip ty2 then t1 else Par(t1,t2)
                  | _ -> Par(t1,t2))
	| _, Basic(_,ty2) -> (
            if Types.isSkip ty2 then t1 else 
                match t1 with
                  Basic(_,ty1) -> if Types.isSkip ty1 then t2 else Par(t1,t2)
                  | _ -> Par(t1,t2))
	| _, _ -> Par(t1,t2)

let mkSeq t1 t2 = 
	match t1, t2 with
	| Skip, _ -> t2
	| _, Skip -> t1
	| Basic(_,ty1), _ -> (
            if Types.isSkip ty1 then t2 else 
                match t2 with
                  Basic(_,ty2) -> if Types.isSkip ty2 then t1 else Seq(t1,t2)
                  | _ -> Seq(t1,t2))
	| _, Basic(_,ty2) -> (
            if Types.isSkip ty2 then t1 else 
                match t1 with
                  Basic(_,ty1) -> if Types.isSkip ty1 then t2 else Seq(t1,t2)
                  | _ -> Seq(t1,t2))
	| _, _ -> Seq(t1,t2)

