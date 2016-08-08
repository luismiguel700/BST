open List;;

type ty =
| SkipTy
| HoleTy of int
| VarTy of int
| SomeTy of some ref
| FunTy of ty * ty
| BasicTy of int
| SeqTy of ty * ty
| ParTy of ty * ty
and
some =
| None
| Some of ty
;;

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| SkipTy -> true
	| HoleTy(_) -> true
	| VarTy(_) -> false
	| SomeTy(_) -> false
	| FunTy(_) -> false
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> isSkip a1 && isSkip a2
	| ParTy(a1,a2) -> isSkip a1 && isSkip a2

(* A=C[t] *)
let rec inFst t a = 
	match a with
	| SkipTy -> false
	| HoleTy(_) -> t=a
	| VarTy(_) -> t=a
	| SomeTy(_) -> false
	| FunTy(_, _) -> (match t with FunTy(_,_) -> true | _ -> false)
	| BasicTy(_) -> t=a
	| SeqTy(a1,a2) -> inFst t a1 || ( isSkip a1  && inFst t a2)
	| ParTy(a1,a2) -> inFst t a1 || inFst t a2 
;;

(* A=A'[~x] ? *) (* optmimizar mais tarde *)
let rec containsVars a xs =
	match a with
	| SkipTy -> false
	| HoleTy(_) -> false
	| VarTy(id) -> mem id xs
	| SomeTy(_) -> false
	| FunTy(_, _) -> false
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> containsVars a1 xs || containsVars a2 xs
	| ParTy(a1,a2) -> containsVars a1 xs || containsVars a2 xs

let rec consistsOfVars a vars =
	match a with
	| SkipTy -> true
	| HoleTy(_) -> false
	| VarTy(id) -> mem id vars
	| SomeTy(_) -> false
	| FunTy(t, u) -> consistsOfVars t vars && consistsOfVars u vars
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars
	| ParTy(a1,a2) -> consistsOfVars a1 vars && consistsOfVars a2 vars

let rec consistsOfVarsEnv a env =
	match a with
	| BasicTy(_) -> false
	| SkipTy -> true
	| SeqTy(a1,a2) -> consistsOfVarsEnv a1 env && consistsOfVarsEnv a2 env
	| ParTy(a1,a2) -> consistsOfVarsEnv a1 env && consistsOfVarsEnv a2 env
	| FunTy(t, u) -> consistsOfVarsEnv t env && consistsOfVarsEnv u env
	| VarTy(id) -> exists (fun (x,y) -> x = id) env
	| HoleTy(_) -> false
	| SomeTy(_) -> false


(* A{C/b} *)
let rec subst a b c =
	match a with
	| SkipTy -> a
	| HoleTy(_) -> if a=b then c else a
	| VarTy(_) -> if a=b then c else a
	| SomeTy(_) -> a
	| FunTy(_, _) -> if a=b then c else a
	| BasicTy(_) -> if a=b then c else a
	| SeqTy(a1,a2) -> SeqTy(subst a1 b c, subst a2 b c)
	| ParTy(a1,a2) -> ParTy(subst a1 b c, subst a2 b c)

let mkPar t1 t2 = 
	match t1, t2 with
	| SkipTy, _ -> t2
	| _, SkipTy -> t1
	| _, _ -> ParTy(t1,t2)

let mkSeq t1 t2 = 
	match t1, t2 with
	| SkipTy, _ -> t2
	| _, SkipTy -> t1
	| _, _ -> SeqTy(t1,t2)