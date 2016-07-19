{
 open Parser;;
 let count = ref 0;;
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
| "->" 		  { ARROW }
| "=" 		  { EQ }

| "let" 	  { LET }
| "in" 		  { IN }
| "fun" 	  { FUN }

| ['1'-'9']+ ['0'-'9']* { NAT(int_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']+                      { INT(int_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']* "." ['0'-'9']*       { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| "\"" ['A'-'Z' 'a'-'z' '0'-'9' '_' ' ' '*' '-']* "\""  { STRING(Lexing.lexeme lexbuf) }
| ['A'-'Z' 'a'-'z' '0'-'9' '_']*  { ID(Lexing.lexeme lexbuf) }

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
 
