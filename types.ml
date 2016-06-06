type ty = 
	| BasicTy of string
	| SkipTy
	| SeqTy of ty * ty
	| ParTy of ty * ty;;

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

let rec extr t1 t2 = 1;;