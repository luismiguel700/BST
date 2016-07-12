open Types;;
open Comm;;
open Test;;
open List;;	

let print_extract a b =
	print_string "extract(";
	print_type a;
	print_string ", ";
	print_type b;
	print_string ")\n"

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
			print_string "] )"
	)
	res

let extr_comm a b = 
	let res = extract a b in
		if length res = 0 then 
			print_string "no solution\n"
		else
			print_extr a b res

let extract_ok_comm a b =
	let res = extract a b in
		if length res = 0 then 
		(
			print_string "ERROR: should not be possible to ";
			print_extract a b
		)
		else
			print_extract a b

let extract_ko_comm a b =
	let res = extract a b in
		if length res = 0 then 
			print_extract a b
		else
		(
			print_string "ERROR: should be possible to ";
			print_extract a b
		)	

let print_join xs a h y a' y' b =
	print_string "join( [";
	iter (fun x -> print_int x; print_string ",") xs ;
	print_string "], ";
	print_type a;
	print_string ", [";
	iter (fun (x, b) -> print_int x; print_string "->"; print_type b; print_string ",") h;
	print_string "], ";
	print_int y;
	print_string " ) = ( ";
	print_type a';
	print_string ", ";
	print_int y';
	print_string "->(";
	print_type b;
	print_string ") )"

let join_comm xs a h y =
	let (a', (y', b)) = join xs a h y in
	(	
		print_join xs a h y a' y' b;
		print_string "\n"
	)

let join_ok_comm xs a h y c =
	let (a', (y', b)) = join xs a h y in
		let res = extract a' c in
			if length res = 0 then
			(
				print_string "ERROR: the result of ";
				print_join xs a h y a' y' b;
				print_string " is not subtype of ";
				print_type c;
				print_string "\n"
			)
			else
			(
				print_join xs a h y a' y' b;
				print_string " , ";
				print_type c;
				print_string "\n"
			)

let rec top_level lexbuf =
	print_string "> " ;
	flush stdout;
	(
		try
			let s = Parser.main Lexer.token lexbuf in 
			(
				match s with
				| Quit -> ()
				| Extract(a,b) -> extr_comm a b; top_level lexbuf
				| OKextract(a,b) -> extract_ok_comm a b; top_level lexbuf
				| KOextract(a,b) -> extract_ko_comm a b; top_level lexbuf
				| Join(xs, a, h, y) -> join_comm xs a h y; top_level lexbuf
				| OKjoin(xs, a, h, y, b) -> join_ok_comm xs a h y b; top_level lexbuf
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
