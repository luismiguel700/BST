open Types;;
open Assertions;;
open Exp;;

let rec print_type_lev t l = 
	match t with
	| SkipTy -> print_string "0"
	| HoleTy(id) -> print_string ("0_"^(string_of_int id))
	| VarTy(id) -> print_int id
	| SomeTy(pointer) ->
		(
			match !pointer with
			| None -> print_string "some"
			| Some(t) -> print_type_lev t 2
		)
	| FunTy(a, b) -> 
		print_string "(";
		print_type_lev a 0;
		print_string "->";
		print_type_lev b 0;
		print_string ")";
	| BasicTy(id) -> print_string (Hashtbl.find Lexer.tableIntStr id)
	| SeqTy(a, b) -> 
		if l>1 then
			print_string "(";
		print_type_lev a 1;
		print_string ";";
		print_type_lev b 1;
		if l>1 then
			print_string ")"
	| ParTy(a,b) -> 
		if l>0 then 
			print_string "(";
		print_type_lev a 0;
		print_string "|";
		print_type_lev b 0;
		if l>0 then
			print_string ")"
;;

let print_type t = print_type_lev t 0;;

let rec print_assertion_lev t l = 
	match t with
	| Skip -> print_string "0"
	| Hole(id) -> print_string ("0_"^(string_of_int id))
	| Var(id) -> print_int id
	| Basic(id, t) -> 
		print_string (Hashtbl.find Lexer.tableIntStr id);
		print_string ":";
		print_type t;
	| Seq(a, b) -> 
		print_assertion_lev a 1;
		print_string ";";
		print_assertion_lev b 1;
	| Par(a,b) -> 
		if (l>0) then 
			print_string "(";
		print_assertion_lev a 0;
		print_string "|";
		print_assertion_lev b 0;
		if (l>0) then
			print_string ")"
;;

let print_assertion t = print_assertion_lev t 0;;

let rec print_exp_lev e l = 
	match e with
	| Id(id) -> print_string (Hashtbl.find Lexer.tableIntStr id)
	| Fun(id, t, e1) -> 
		print_string "fun ";
		print_string (Hashtbl.find Lexer.tableIntStr id);
		print_string " -> ";
		print_exp_lev e1 0
	| Let(id, e1, e2) ->
		print_string "let ";
		print_string (Hashtbl.find Lexer.tableIntStr id);
		print_string " = ";
		print_exp_lev e1 0;
		print_string " in ";
		print_exp_lev e2 0
	| Call(e1, e2) ->
		print_exp_lev e1 0;
		print_string " ";
		print_exp_lev e2 0
	| Select(e1, id) ->
		print_exp_lev e1 0;
		print_string ("."^(Hashtbl.find Lexer.tableIntStr id));
;;

let print_exp e = print_exp_lev e 0;;
