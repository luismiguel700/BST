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
	| FunTy(_, _) -> Basic(id, t)
	| BasicTy(id') -> Basic(id, BasicTy(id'))
	| SeqTy(a1, a2) -> Seq(makeAssertion id a1, makeAssertion id a2)
	| ParTy(a1, a2) -> Par(makeAssertion id a1, makeAssertion id a2)
	| _ -> raise (Fail("makeAssertion: illegal call"))


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
	| Hole(_) -> true
	| Var(_) -> false
	| Basic(_, t) -> Types.isSkip t
	| Seq(a1,a2) ->isSkip a1 && isSkip a2
	| Par(a1,a2) -> isSkip a1 && isSkip a2

(* A=C[t] *)

(*

let rec inFst t a = 
	match a with
	| Skip -> false
	| Seq(a1,a2) -> inFst t a1 || (isSkip a1 && inFst t a2)
	| Par(a1,a2) -> inFst t a1 || inFst t a2 
	| Basic(id,_) -> (match t with Basic(id',_) -> id = id' | _ -> false)
	| Hole(_) -> false
	| Var(_) -> false
;;

*)

let rec inFst_act t a = 
	match a with
	| Seq(a1,a2) -> inFst_act t a1 || (isSkip a1 && inFst_act t a2)
	| Par(a1,a2) -> inFst_act t a1 || inFst_act t a2 
	| Basic(id, _) -> id=t
        | _ -> false
;;


(* A=A'[~x] ? *) (* optmizar mais tarde *)
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
	| Var(id) -> mem id vars
	| Basic(_, t) -> Types.consistsOfVars t vars
	| Seq(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars
	| Par(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars

let rec consistsOfVarsEnv a env =
	match a with
	| Skip -> true
	| Hole(_) -> true
	| Var(id) -> exists (fun (x,y) -> x=id) env
	| Basic(_, t) -> Types.isSkip t (* Types.consistsOfVarsEnv t env *) (* NB. in assertions, t must be var free *)
	| Seq(a1,a2) -> consistsOfVarsEnv a1 env && consistsOfVarsEnv a2 env
	| Par(a1,a2) -> consistsOfVarsEnv a1 env && consistsOfVarsEnv a2 env


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

