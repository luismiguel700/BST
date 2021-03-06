{
 open Parser;;
 let count = ref 0;;
 let tableStrInt = Hashtbl.create 100;;
 let tableIntStr = Hashtbl.create 100;;

 (* if there is a i->s in the hashtable, then it creates an entry length->s$length and returns length *)
 let freshId(i:int):int = 
		let s = Hashtbl.find tableIntStr i in
		let len = Hashtbl.length tableIntStr in (* the hashtable is always growing *)
			let s_len = s^"$"^(string_of_int len) in
			(	
				Hashtbl.add tableStrInt s_len len;
				Hashtbl.add tableIntStr len s_len; 
				len
			)		
}

rule token = parse
  [' ' '\t' '\n' '\r']  { token lexbuf }
| "/*"             { incr count; comment lexbuf }
| "//"             { linecomment lexbuf } 
| "#"             { linecomment lexbuf } 

| eof   			{ EOF }

| "extr"       { EXTR }
| "extr_a"       { EXTR_A }
| "OKextr"       { OK_EXTR }
| "KOextr"       { KO_EXTR }
| "join"       { JOIN }  
| "OKjoin"     { OK_JOIN } 
| "type"     { TYPE } 
| "typecheck"     { CTYPE } 
| "quit"          { QUIT } 

| "bool"          { BOOLT }
| "int"           { INTT }
| "!"             { BANG }
| "0"          { STOPT }
| "|"  		  { PAR }
| ";"  		  { SEQ }
| ":"		{ COLON }
| "!"  		  { BANG }
| "<:" 		  { SUBT }

| "." 		  { DOT }
| "," 		  { COMMA }
| ";;" 		  { TERM }
| "::" 		  { DOUBLE_COLON }
| "(" 		  { LPAR }
| ")" 		  { RPAR }
| "[" 		  { LPAR2 }
| "]" 		  { RPAR2 }
| "{" 		  { LPAR3 }
| "}" 		  { RPAR3 }
| "->" 		  { ARROW }
| "=" 		  { EQ }

| "let" 	  { LET }
| "in" 		  { IN }
| "fun" 	  { FUN }
| "some" 	  { SOME }	

| ['1'-'9']+ ['0'-'9']* { NAT(int_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']+                      { INT(int_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']* "." ['0'-'9']*       { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| "\"" ['A'-'Z' 'a'-'z' '0'-'9' '_' ' ' '*' '-']* "\""  { STRING(Lexing.lexeme lexbuf) }
| ['A'-'Z' 'a'-'z' '0'-'9' '_']*  
	{ 
		let s = Lexing.lexeme lexbuf in
			if Hashtbl.mem tableStrInt s then
				ID(Hashtbl.find tableStrInt s)
			else
				let i = Hashtbl.length tableStrInt in (* the hashtable is always growing *)
				(	
					Hashtbl.add tableStrInt s i;
					Hashtbl.add tableIntStr i s; 
					ID(i) 
				)
	}

| _ { raise Parsing.Parse_error }

and comment = parse
  ['\t' '\n']                 { comment lexbuf }
| "*/"                        { decr count; if !count = 0 then token lexbuf else comment lexbuf }
| "/*"                        { incr count; comment lexbuf }
| [' '-')' '+'-'.' '0'-'~']*  { comment lexbuf }
| '*'+ [' '-'.' '0'-'~']      { comment lexbuf }
| '/'+ [' '-')' '+'-'~']      { comment lexbuf }

and linecomment = parse
  ['\n']                      { token lexbuf }
| _                           { linecomment lexbuf }
 
