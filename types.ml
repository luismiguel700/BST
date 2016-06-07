type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty
;;

exception FailId of string*string;;
exception FailEmptyRes;;

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

let rec extr a b = 
	match a,b with
	| _, SkipTy -> (a,b)
	| SkipTy, BasicTy(id) -> (a, b)
	| BasicTy(id), BasicTy(id') when id=id' -> (SkipTy,SkipTy)
	| BasicTy(id), BasicTy(id') -> raise (FailId(id,id'))
	| SeqTy(a1,a2), BasicTy(id) -> extrSeqId a1 a2 b
	| _, SeqTy(b1,b2) -> extrSeq a b1 b2

(* pre-condition: b is a BasicTy *)
and extrSeqId a1 a2 b = 
	let (a1', b') = extr a1 b in
		match a1',b' with
		| _, SkipTy -> (SeqTy(a1',a2), b')
		| SkipTy, _ -> extr a2 b'
		| _, _ -> raise FailEmptyRes

and extrSeq a b1 b2 =
	let (a', b1') = extr a b1 in
		match a', b1' with
		| SkipTy, _ -> (a', SeqTy(b1',b2))
		| _, SkipTy -> extr a' b2
		| _, _ -> raise FailEmptyRes	
;;
