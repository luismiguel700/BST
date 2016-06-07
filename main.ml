open Types;;
open Comm;;																
let rec top_level lexbuf =
  print_string "> " ;
  flush stdout;
  (
      try
        let s = Parser.main Lexer.token lexbuf
        in 
        (
  	    	match s with
  	        | Quit -> ()
			| Extract(a,b) -> 
				let (a',b') = extr a b in
					print_string "extr(";
                	print_type a;
                	print_string ",";
                	print_type b;
                	print_string ")=(";
                	print_type a';
                	print_string ",";
                	print_type b';
                	print_string ")\n";
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
