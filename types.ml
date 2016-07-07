open List;;

type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty
	| Var of int
	| Hole of int
;;

type map = (int * ty) list

exception FailId of string*string;;
exception FailVarId of int*string;;
exception FailEmptyRes;;
exception Fail of string;;

let idCount = ref 0;;

let resetCount () = idCount := 0

let freshId () = (idCount := (!idCount+1)); !idCount

let rec print_type_lev t l = 
	match t with
	| SkipTy -> print_string "0"
	| BasicTy(id) -> print_string id
	| Var(id) -> print_int id
	| Hole(id) -> print_string ("0_"^(string_of_int id))
	| SeqTy(a, b) -> 
		print_type_lev a 1;
		print_string ";";
		print_type_lev b 1;
	| ParTy(a,b) -> 
		if (l>0) then 
			print_string "(";
			print_type_lev a 0;
			print_string "|";
			print_type_lev b 0;
		if (l>0) then
			print_string ")"
;;

let print_type t = print_type_lev t 0;;

(* A<:>0 ? *)
let rec isSkip a =
	match a with
	| SkipTy -> true
	| BasicTy(_) -> false
	| Var(_) -> false
	| Hole(_) -> true
	| SeqTy(a1,a2) -> isSkip a1 && isSkip a2
	| ParTy(a1,a2) -> isSkip a1 && isSkip a2

(* A=C[t] *)
let rec inFst t a = 
	match a with
	| SkipTy -> false
	| BasicTy(_) -> t=a
	| Var(_) -> t=a
	| Hole(_) -> false
	| SeqTy(a1,a2) -> 
		if isSkip a1 then
			inFst t a2
		else
			inFst t a1
	| ParTy(a1,a2) -> inFst t a1 || inFst t a2 
;;

(* A{C/b} *)
let rec subst a b c =
	match a with
	| SkipTy -> a
	| BasicTy(_) -> if a=b then c else a
	| Var(_) -> if a=b then c else a
	| Hole(_) -> if a=b then c else a
	| SeqTy(a1,a2) -> SeqTy(subst a1 b c, subst a2 b c)
	| ParTy(a1,a2) -> ParTy(subst a1 b c, subst a2 b c)

let rec substVarsHoles a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substVarsHoles (subst a (Var(id)) (Hole(id))) tail

let rec substHolesVars a h =
	match h with
	| [] -> a
	| (id,_)::tail -> substHolesVars (subst a (Hole(id)) (Var(id))) tail

let rec extr(a:ty)(b:ty):((ty * ty * map) list) = 
	match a,b with
	| _, SkipTy -> [(a, b, [])]
	| SkipTy, BasicTy(id) -> [(a, b, [])]
	| Hole(_), BasicTy(id) -> [(a, b, [])]
	| BasicTy(id), BasicTy(id') when id=id' -> extrIdId id
	| BasicTy(id), BasicTy(id') -> []
	| Var(id), BasicTy(id') -> []
	| SeqTy(a1,a2), BasicTy(id) -> extrSeqId a1 a2 b
	| ParTy(a1,a2), BasicTy(id) -> extrParId a1 a2 b
	| _, SeqTy(b1,b2) -> extrSeq a b1 b2
	| _, ParTy(b1,b2) -> extrPar a b1 b2
	| _, _ -> raise (Fail("not defined"))

and extrIdId id =
	let newId = freshId () in
		[(Var(newId), SkipTy, [(newId, BasicTy(id))])]

and extrSeqId a1 a2 b = 
	let a1s = extr a1 b in
		concat
		(
			map
			(
				fun (a1', b', h) ->
					match b' with
					| SkipTy -> [(SeqTy(a1',a2), b', h)]
					| _ ->
						let a2s = extr a2 b' in
							map (fun (a2', b', h) -> (SeqTy(a1', a2'), b', h)) a2s
			)
			a1s
		)

and extrParId a1 a2 b =
	let a1s = extr a1 b in
	let a2s = extr a2 b in
		concat
			(
				map
				(
					fun (a1', b', h1) -> 
						append
						(if b=b' then [] else [(ParTy(a1', a2), SkipTy, h1)])
						(
							map 
							(
								fun (a2', b'', h2) -> 
									if b=b'' then
										(ParTy(a1',a2'), b, [])
									else
										(ParTy(a1, a2'), SkipTy, h2)
							)
							a2s
						)
				)
				a1s
			)

and extrSeq a b1 b2 =
	let a_s = extr a b1 in
		concat
		(
			map
			(
				fun (a', b1', h1) ->
					if isSkip b1' then
						let ah' = substVarsHoles a' h1 in
							let a_s' = extr ah' b2 in
							map
							(
								fun (ah'', b2', h2) ->
									let a'' = substHolesVars ah'' h1 in
										(a'', b2', h1@h2)
							)
							a_s'
					else
						[(a', SeqTy(b1',b2), h1)]
			)
			a_s
		)

and extrPar a b1 b2 =
	let a_s = extr a b1 in
		concat
		(
			map
			(
				fun (a', b1', h1) ->
					let a_s' = extr a' b2 in
						map
						(
							fun (a'', b2', h2) -> (a'', ParTy(b1',b2'), h1@h2)
						)
						a_s'
			)
			a_s
		)
;;
