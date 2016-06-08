type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty
;;

exception FailId of string*string;;
exception FailEmptyRes;;
exception Fail of string;;

let rec print_type_lev t l = 
  match t with
  |  SkipTy -> 
       print_string "0"
  |  BasicTy(id) -> 
       print_string id;
  |  SeqTy(a,b) -> 
       print_type_lev a 1;
       print_string ";";
	  print_type_lev b 1;
  |  ParTy(a,b) -> 
       if (l>0) then print_string "(";
       print_type_lev a 0;
       print_string "|";
	  print_type_lev b 0;
       if (l>0) then print_string ")"
;;

let print_type t = print_type_lev t 0;;

(* A<:0 ? *)
let rec isSkip a =
	match a with
	| SkipTy -> true
	| BasicTy(_) -> false
	| SeqTy(a1,a2) -> isSkip a1 && isSkip a2
	| ParTy(a1,a2) -> isSkip a1 && isSkip a2

(* exists C. C[id]=A *)
let rec inFst id a = 
	match a with
	| SkipTy -> false
	| BasicTy(id') -> id=id'
	| SeqTy(a1,a2) -> 
		if isSkip a then
			inFst id a2
		else
			inFst id a1
	| ParTy(a1,a2) -> inFst id a1 || inFst id a2 
;;

let rec extr a b = 
	match a,b with
	| _, SkipTy -> (a,b)
	| SkipTy, BasicTy(id) -> (a, b)
	| BasicTy(id), BasicTy(id') when id=id' -> (SkipTy,SkipTy)
	| BasicTy(id), BasicTy(id') -> raise (FailId(id,id'))
	| SeqTy(a1,a2), BasicTy(id) -> extrSeqId a1 a2 id
	| ParTy(a1,a2), BasicTy(id) -> extrParId a1 a2 id
	| _, SeqTy(b1,b2) -> extrSeq a b1 b2
	| BasicTy(id), ParTy(b1,b2) -> extrIdPar id b1 b2
(*	| SeqTy(a1,a2), ParTy(b1,b2) -> extrSeqPar a1 a2 b1 b2 *)

and extrSeqId a1 a2 id = 
	let (a1', b') = extr a1 (BasicTy(id)) in
		match a1', b' with
		| _, SkipTy -> (SeqTy(a1',a2), b')
		| SkipTy, _ -> extr a2 b'
		| _, _ -> raise FailEmptyRes

and extrParId a1 a2 id =
	if inFst id a1 && inFst id a2 then
		let (a1',_) = extr a1 (BasicTy(id)) in
		let (a2',_) = extr a2 (BasicTy(id)) in
			(ParTy(ParTy(a1',a2'),BasicTy(id)),SkipTy)
	else if inFst id a1 then
		let (a1',_) = extr a1 (BasicTy(id)) in
			(ParTy(a1',a2),SkipTy)
	else if inFst id a2 then
		let (a2',_) = extr a2 (BasicTy(id)) in
			(ParTy(a1,a2'),SkipTy)
	else
		let (a1',b) = extr a1 (BasicTy(id)) in
		let (a2',c) = extr a2 (BasicTy(id)) in
			match a1',a2' with
			| SkipTy, SkipTy -> (SkipTy, BasicTy(id))
			| _, _ -> raise FailEmptyRes

and extrSeq a b1 b2 =
	let (a', b1') = extr a b1 in
		match a', b1' with
		| SkipTy, _ -> (a', SeqTy(b1',b2))
		| _, SkipTy -> extr a' b2
		| _, _ -> raise FailEmptyRes

and extrIdPar id b1 b2 =
	if isSkip b1 && isSkip b2 then
		(BasicTy(id), SkipTy)
	else if isSkip b1 then
		extr (BasicTy(id)) b2
	else if isSkip b2 then
		extr (BasicTy(id)) b1
	else
		raise (Fail("extrIdPar"))
;;
