open Types;;
open Assertions;;
open Exp;;

let level0 = 0;;
let level1 = 1;;
let level2 = 2;;
let level3 = 3;;

let rec print_type_lev t l = 
	match t with
	| SkipTy -> print_string "0"
	| HoleTy(id) -> print_string ("0_"^(string_of_int id))
	| VarTy(id) -> print_int id
	| SomeTy(pointer) ->
		(
			match !pointer with
			| None -> print_string "some"
			| Some(t) -> print_type_lev t level2
		)
	| FunTy(a, b) -> 
		if l>=level2 then
			print_string "(";
		print_type_lev a level2;
		print_string "->";
		print_type_lev b level2;
		if l>=level2 then
			print_string ")";
	| BasicTy(id) -> print_string (Hashtbl.find Lexer.tableIntStr id)
	| SeqTy(a, b) -> 
		if l>level1 then
			print_string "(";
		print_type_lev a level1;
		print_string ";";
		print_type_lev b level1;
		if l>level1 then
			print_string ")"
	| ParTy(a,b) -> 
		if l>level0 then 
			print_string "(";
		print_type_lev a level0;
		print_string "|";
		print_type_lev b level0;
		if l>level0 then
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
		print_assertion_lev a level1;
		print_string ";";
		print_assertion_lev b level1;
	| Par(a,b) -> 
		if (l>level0) then 
			print_string "(";
		print_assertion_lev a level0;
		print_string "|";
		print_assertion_lev b level0;
		if (l>level0) then
			print_string ")"
;;

let print_assertion t = print_assertion_lev t 0;;

let rec print_exp_lev e l = 
	match e with
	| Id(id) -> print_string (Hashtbl.find Lexer.tableIntStr id)
	| Fun(id, tArg, tRet, e1) -> 
		if l>level0 then
			print_string "(";
		print_string "fun (";
		print_string (Hashtbl.find Lexer.tableIntStr id);
		print_string (":");
		print_type tArg;
		print_string ") -> ";
		print_type tRet;
		print_string " { ";
		print_exp_lev e1 level0;
		print_string " }";
		if l>level0 then
			print_string ")";
	| Let(id, t, e1, e2) ->
		if l>level0 then
			print_string "(";
		print_string "let ";
		print_string (Hashtbl.find Lexer.tableIntStr id);
		print_string (":");
		print_type t;
		print_string " = ";
		print_exp_lev e1 level0;
		print_string " in ";
		print_exp_lev e2 level0;
		if l>level0 then
			print_string ")";

	| Seqe(e1, e2) ->
		if l>level0 then 
			print_string "(";
		print_exp_lev e1 level0;
			print_string " ; ";
		print_exp_lev e2 level0;
		if l>level0 then
			print_string ")";

	| Call(e1, e2) ->
		if l>level2 then
			print_string "(";
		print_exp_lev e1 level1;
		print_string " ";
		print_exp_lev e2 level1;
		if l>level2 then
			print_string ")"
	| Select(e1, id) ->
		print_exp_lev e1 level2;
		print_string ("."^(Hashtbl.find Lexer.tableIntStr id));

;;

let print_exp e = print_exp_lev e 0;;

let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";;

