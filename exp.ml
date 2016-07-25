open Types;;

type exp =
| Id of int
| Fun of int * ty * ty * exp
| Call of exp * exp
| Let of int * ty * exp * exp
| Select of exp * int
;;

let rec substId e x y =
	match e with
	| Id(id) -> if id = x then Id(y) else e
	| Fun(id, tArg, tRet, e1) -> if id = x then e else Fun(id, tArg, tRet, substId e1 x y)
	| Call(e1, e2) -> Call(substId e1 x y, substId e2 x y)
	| Let(id, t, e1, e2) -> 
		if id = x then 
			Let(id, t, substId e1 x y, e2) 
		else 
			Let(id, t, substId e1 x y, substId e2 x y)
	| Select(e1, id) -> Select(substId e1 x y, id)
