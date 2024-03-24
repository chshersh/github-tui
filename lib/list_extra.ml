let in_between ~sep list =
  let rec loop = function
    | [] -> []
    | x :: xs -> sep :: x :: loop xs
  in
  match list with
  | [] | [ _ ] -> list
  | x :: xs -> x :: loop xs

let generate n f =
  let rec loop i = if i = n then [] else f i :: loop (i + 1) in
  loop 0

let rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs
