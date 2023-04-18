let is_square a2 =
	let sr = float_of_int a2 |> sqrt |> int_of_float in
	sr * sr = a2, sr

let is_pythagorean b c =
	let a2 = c*c - b*b in
	let bl, a = is_square a2 in
	bl, a

let rec seek b c =
	let bl, a = is_pythagorean b c in
	let newb = min (c-2) (1000 - c) in
	if c < 333 then failwith "Not found"
	else
	match b, bl, a + b + c with
	| 0, _, _ -> 
		print_int newb ; print_string " | " ; print_int c ; print_newline() ;
		seek newb (c - 1)
	| _, true, 1000 -> (a, b, c)
	| _, _, _ -> seek (b-1) c

let () = 
	let a, b, c = seek 997 998 in
	print_int (a*b*c) ; print_newline()