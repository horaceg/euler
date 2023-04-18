(* let rec pow a n = match n, n mod 2 with
   	| 0, _ -> 1
   	| _, 0 -> let x = pow a (n/2) in x*x
   	| _, _ -> a * pow a (n-1)

   let primes = [2; 3; 5; 7; 11; 13; 17; 19]

   let rec maxpow n acc =
   	if acc > 20 then -1
   	else 1 + maxpow n (acc*n)

   let rec exponents = function
   	| [] -> []
   	| p::t -> maxpow p 1 :: exponents t

   let () =
   	exponents primes
   	|> List.fold_left2 (fun a b c -> a * (pow b c)) 1 primes
   	|> print_int ; print_newline()
*)

(* Enter your code here. Read input from STDIN. Print output to STDOUT *)

(* Enter your code here. Read input from STDIN. Print output to STDOUT *)

let rec pow a n =
  match (n, n mod 2) with
  | 0, _ -> 1
  | _, 0 ->
      let x = pow a (n / 2) in
      x * x
  | _, _ -> a * pow a (n - 1)

let primes = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37 ]

let rec maxpow bound n acc =
  if acc > bound then -1 else 1 + maxpow bound n (acc * n)

let rec exponents bound = function
  | [] -> []
  | p :: t -> maxpow bound p 1 :: exponents bound t

let rec solve_it = function
  | 0 -> ()
  | k ->
      let n = int_of_string (input_line stdin) in
      let bounded_primes = List.filter (fun p -> p <= n) primes in
      bounded_primes |> exponents n
      |> List.fold_left2 (fun a b c -> a * pow b c) 1 bounded_primes
      |> print_int;
      print_newline ();
      solve_it (k - 1)

let () =
  let t = int_of_string (input_line stdin) in
  solve_it t
