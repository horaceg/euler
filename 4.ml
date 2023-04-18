let list_of_int n = (* careful, the int is reversed for efficiency reasons*)
	let rec aux = function
		| 0 -> []
		| n -> n mod 10 :: aux (n/10)
	in aux n

let is_palindrome n = 
	let l = list_of_int n in
	let lr = List.rev l in
	l = lr

(* \forall k <= 5, n/10^k mod 10 = n/10^(n-k) mod 10 *)

let rec seek n a b = (* here we choose a >= b *)
	match a / 100, b / 100 with
	| 0, _ -> n
	| _, 0 -> seek n (a-1) (a-1)
	| _, _ -> 
		let m = a*b in
		let newn = 
			if is_palindrome m then 
				if m > n then begin
					print_int m ; print_newline() ;
					m
				end
				else n
			else n
		in 
		(* print_int a ; print_string " " ; print_int b ; print_string " " ; print_int m ; print_string " " ; print_int n ; print_newline() ; *)
		seek newn a (b-1)

let () = 
	seek 0 999 999 |> print_int ; print_newline()