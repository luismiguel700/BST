let rec typecheck a e t =
	match e, t with
	| Fun(x, e1), FunTy(argTy, retTy) ->
		typecheck (ParTy(a, Basic(x, argTy))) e1 retTy
	| Fun(_, _), _ -> raise (Fail("not a function type"))
	| Call(e1, e2), _ ->
		let (a', h1, t1) = typecheck a e1 (FunTy(Top, t)) in
			match t1 with
			| Fun(argType, retType) ->
				if retType = t then (* retType <: t *)
				(
					let (a'', h2, t2) = typecheck a' e2 argType
					let newId = freshId() in
						let (a''', (_, t'')) = join h1@h2 a'' newId in
							(a''', (newId))
				)
				else
					raise (Fail("wrong return type"))
			| _ -> raise (Fail("not a function type"))
	| Let(id, e1, e2) ->
		let (a', t1) = typecheck a e1 Top in

	| Select(e1, id) ->
		let
