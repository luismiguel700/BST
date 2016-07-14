open Types;;
open Extract;;
open Comm;;
open Test;;
open List;;	

let print_extract (a:ty)(b:ty):unit =
	print_string "extract(";
	print_type a;
	print_string ", ";
	print_type b;
	print_string ")\n"

let print_extr (a:ty)(b:ty)(a':ty)(b':ty)(h:map):unit =
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

let extr_cps_stack_comm (a:ty)(b:ty):unit = 
	init a b;
	while hasNext () do
		try
			let (a', b', h) = next () in
				print_extr a b a' b' h
		with
		| Fail(_) -> ()
	done

let extr_cps_comm (a:ty)(b:ty):unit = 
	try
		let (a', b', h) = Extract.extract a b in
			print_extr a b a' b' h
	with
	| Fail(s) -> print_string (s^"\n")

let ok_extr_cps_comm (a:ty)(b:ty):unit = 
	try
		let _ = Extract.extract a b in
			print_extract a b
	with
	| Fail(_) -> print_string "ERROR: it should not be possible to "; print_extract a b

let ko_extr_cps_comm (a:ty)(b:ty):unit = 
	try
		let _ = Extract.extract a b in
			print_string "ERROR: it should not be possible to "; print_extract a b
	with
	| Fail(_) -> print_extract a b

let extr_comm (a:ty)(b:ty):unit = 
	let res = Types.extract a b in
		if length res = 0 then 
			print_string "no solution\n"
		else
			iter (fun (a', b', h) -> print_extr a b a' b' h) res

let ok_extract_comm (a:ty)(b:ty):unit =
	let res = Types.extract a b in
		if length res = 0 then 
		(
			print_string "ERROR: it should not be possible to ";
			print_extract a b
		)
		else
			print_extract a b

let ko_extract_comm (a:ty)(b:ty):unit =
	let res = Types.extract a b in
		if length res = 0 then 
			print_extract a b
		else
		(
			print_string "ERROR: it should be possible to ";
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

let ok_join_comm xs a h y c =
	let (a', (y', b)) = join xs a h y in
		let res = Types.extract a' c in
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
				| Extract(a,b) -> extr_cps_stack_comm a b; top_level lexbuf
				| OKextract(a,b) -> ok_extr_cps_comm a b; top_level lexbuf
				| KOextract(a,b) -> ko_extr_cps_comm a b; top_level lexbuf
				| Join(xs, a, h, y) -> join_comm xs a h y; top_level lexbuf
				| OKjoin(xs, a, h, y, b) -> ok_join_comm xs a h y b; top_level lexbuf
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
