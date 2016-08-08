open Exceptions;;
open Types;;
open Assertions;;
open Exp;;
open Extract;;
open Extract_a;;
open Join;;
open Typecheck;;
open Comm;;
open List;;
open Unparser;;

let print_extract (a:ty)(b:ty):unit =
	print_string "extract(";
	print_type a;
	print_string ", ";
	print_type b;
	print_string ")\n"

let print_extr (a:ty)(b:ty)(a':ty)(b':ty)(h:Extract.map):unit =
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

let print_extract_a (a:assertion)(b:assertion):unit =
	print_string "extract(";
	print_assertion a;
	print_string ", ";
	print_assertion b;
	print_string ")\n"

let print_extr_a (a:assertion)(b:assertion)(a':assertion)(b':assertion)(h:Extract_a.map):unit =
	print_string "extr( ";
	print_assertion a;
	print_string ", ";
	print_assertion b;
	print_string " ) = ( ";
	print_assertion a';
	print_string ", ";
	print_assertion b';
	print_string ", [";
	iter 
	(
		fun (id, t) -> 
			print_int id; 
			print_string "->"; 
			print_assertion t; 
			print_string ","
	) 
	h ;
	print_string "] )\n"

let extr_comm (a:ty)(b:ty):unit = 
	Extract.init a b (fun (a', b', h) -> print_extr a b a' b' h);

	let solution = ref false in
	(
		while Extract.hasNext () do
			try
				Extract.next ();
				solution := true
			with
			| Fail(s) -> () (*print_string (s^"\n")*)
		done;

		if not !solution then
		(
			print_string "Failed to "; 
			print_extract a b
		)
	)

let extr_is_ok (a:ty)(b:ty):bool = 
	Extract.init a b (fun (a', b', h) -> ());
	let ok = ref false in
	(
		while Extract.hasNext () && (not !ok) do
			try
				Extract.next ();
				ok := true;
			with
			| Fail(s) -> () (*print_string (s^"\n")*)
		done;
		!ok
	)

let ok_extr_comm (a:ty)(b:ty):unit = 
	let res = extr_is_ok a b in
		if res then
			print_extract a b
		else
		(	
			print_string "ERROR: the following extraction should have succeeded "; 
			print_extract a b
		)

let ko_extr_comm (a:ty)(b:ty):unit = 
	let res = extr_is_ok a b in
		if res then
		(	
			print_string "ERROR: the following extraction should not have succeeded "; 
			print_extract a b
		)
		else
			print_extract a b

let extr_a_comm (a:assertion)(b:assertion):unit =
	Extract_a.init a b (fun (a', b', h) -> print_extr_a a b a' b' h);
	while Extract_a.hasNext () do
		try
			Extract_a.next ()
		with
		| Fail(s) -> () (*print_string (s^"\n")*)
	done

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
		let res = extr_is_ok a' c in
			if res then
			(
				print_join xs a h y a' y' b;
				print_string " , ";
				print_type c;
				print_string "\n"
			)
			else
			(
				print_string "ERROR: the result of ";
				print_join xs a h y a' y' b;
				print_string " is not subtype of ";
				print_type c;
				print_string "\n"
			)

let print_typecheck a e t a'  =
(*	print_string "type(";
	print_assertion a;
	print_string ", ";
	print_exp e;
	print_string ", ";
	print_type t;
	print_string ") = "; *)
	print_string "Yes : ";
	print_assertion a';
	print_string " " ;
	let _= read_line () in print_string "\n"


let typecheck_comm a e t =
	Typecheck.init a e t (fun (a', _) -> print_typecheck a e t a');

	while Typecheck.hasNext () do
		try
		        (* print_string "NEXT:\n"; *)
			Typecheck.next ()
		with
		| Fail(s) -> () (* print_string ("FAIL : "^s^"\n") *)
	done;
	print_string ("No.\n")


let rec top_level lexbuf =
	print_string "> " ;
	flush stdout;
	(
		try
			let s = Parser.main Lexer.token lexbuf in 
			(
				match s with
				| Quit -> ()
				| Extract(a, b) -> extr_comm a b; top_level lexbuf
				| ExtractA(a, b) -> extr_a_comm a b; top_level lexbuf
				| OKextract(a, b) -> ok_extr_comm a b; top_level lexbuf
				| KOextract(a, b) -> ko_extr_comm a b; top_level lexbuf
				| Join(xs, a, h, y) -> join_comm xs a h y; top_level lexbuf
				| OKjoin(xs, a, h, y, b) -> ok_join_comm xs a h y b; top_level lexbuf
				| Typecheck(a, e, t) -> typecheck_comm a e t; top_level lexbuf
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
