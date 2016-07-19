type exp =
| Id of string
| Fun of string * exp
| Call of exp * exp
| Let of string * exp * exp
| Select of exp * string
;;

let rec print_exp_lev e l = 
	match e with
	| Id(s) -> print_string s
	| Fun(s, e1) -> 
		print_string "fun ";
		print_string s;
		print_string " -> ";
		print_exp_lev e1 0
	| Let(s, e1, e2) ->
		print_string "let ";
		print_string s;
		print_string " = ";
		print_exp_lev e1 0;
		print_string " in ";
		print_exp_lev e2 0
	| Call(e1, e2) ->
		print_exp_lev e1 0;
		print_string " ";
		print_exp_lev e2 0
	| Select(e1, s) ->
		print_exp_lev e1 0;
		print_string ("."^s);
;;

let print_exp e = print_exp_lev e 0;;
