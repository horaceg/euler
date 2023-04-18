let is_prime n =
	let sr = float_of_int n |> sqrt |> floor |> int_of_float in
	let rec test = function
		| k when k > sr -> true
		| k -> 
			if n mod k = 0 then false
			else test (k+2)
	in (n mod 2 <> 0 || n = 2) && test 3

let find_next p =
	let rec aux step = 
		if is_prime step then step
		else aux (2 + step)
	in 
	match p with
	| 2 -> 3
	| _ -> aux (p+2)

let rec find remaining last = 
	match remaining with
	| 1 -> last
	| _ -> find (remaining - 1) (find_next last)

let () = find 10001 2 |> print_int ; print_newline()