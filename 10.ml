let is_prime n primes =
	let sr = float_of_int n |> sqrt |> floor |> int_of_float in
	let asc_primes = List.rev primes in
	let rec test = function
		(* | [] -> true *)
		| p :: _ when p > sr -> true
		| p :: t -> 
			if n mod p = 0 then false
			else test t
	in test asc_primes

let find_next primes =
	let rec aux n = 
		if is_prime n primes then n
		else aux (2 + n)
	in
	let p = List.hd primes in
	aux (p+2) :: primes

let rec sum_primes sum primes_so_far bound =
	let p = List.hd primes_so_far in
	if p >= bound then sum
	else sum_primes (sum + p) (find_next primes_so_far) bound

let () = 
	(* Careful: runs for more than 1 hour for 2 million *)
	sum_primes 2 [3; 2] 200000 |> print_int ; print_newline()
