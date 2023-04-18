let rec select = function
  | 0 -> []
  | n -> (
      match n mod 3 * (n mod 5) with
      | 0 -> n :: select (n - 1)
      | _ -> select (n - 1))

let () =
  select 999
  |> List.iter (fun x ->
         print_int x;
         print_string " ");
  print_newline ();
  select 999 |> List.fold_left ( + ) 0 |> print_int
