open Types;;

let a = BasicTy("a");;
let b = BasicTy("b");;
let c = BasicTy("c");;
let d = BasicTy("d");;
let e = BasicTy("e");;
let f = BasicTy("f");;

let x = Var(0);;
let y = Var(1);;
let z = Var(2);;

let ex1 = ParTy(ParTy(SeqTy(ParTy(y,a),b),d), SeqTy(ParTy(z,e),f));;

let test = ();;
