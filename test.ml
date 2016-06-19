open Types;;

let a = BasicTy("a");;
let b = BasicTy("b");;
let c = BasicTy("c");;
let d = BasicTy("d");;
let e = BasicTy("e");;
let f = BasicTy("f");;

let x = Var("x");;
let y = Var("y");;
let z = Var("z");;

let sample1 = ParTy(ParTy(SeqTy(ParTy(y,a),b),d), SeqTy(ParTy(z,e),f));;

let test_join_par =
	print_type sample1; print_string "\n";
	let res = join_par "y" "z" sample1 "x" in
		print_type res;
		print_string "\n"
;;

(*
let test_split =
	print_type sample1; print_string "\n";
	let (res1, res2) = split sample1 "y" in
		print_type res1;
		print_string "\n";
		print_type res2;
		print_string "\n"
;;
*)

let test = test_join_par;;
