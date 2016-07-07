open Types;;
open Comm;;
open Test;;
open List;;	

let print_extr a b res =
	iter
	(
		fun (a',b',h) ->
			print_string "extr( ";
			print_type a;
			print_string ", ";
			print_type b;
			print_string " ) = ( ";
			print_type a';
			print_string ", ";
			print_type b';
			print_string ", [";
			iter 
			(
				fun (id, t) -> 
					print_int id; 
					print_string "->"; 
					print_type t; 
					print_string ","
			) 
			h ;
			print_string "] )\n"
	)
	res

let extract a b = 
	resetCount ();
	try
		let res = extr a b in
			if length res = 0 then 
				print_string "no solution\n"
			else
				print_extr a b res
	with
	| FailId(s,s') -> 
		print_string "incompatible ids: ";
		print_string s;
		print_string " and ";
		print_string s';
		print_string "\n"
	| FailEmptyRes ->
		print_string "invalid invariant: empty residue"
	| Fail(s) ->
		print_string "failed in " ; print_string s; print_string "\n"

let rec top_level lexbuf =
	print_string "> " ;
	flush stdout;
	(
		try
			let s = Parser.main Lexer.token lexbuf in 
			(
				match s with
				| Quit -> ()
				| Extract(a,b) -> 
					extract a b;
					top_level lexbuf 
			)
		with
			Parsing.Parse_error ->
				print_string "Syntax error\n" ;
				top_level lexbuf
	)
;;

let main () = 
	let lexbuf = Lexing.from_channel (stdin) in top_level lexbuf
;;

main ();;
