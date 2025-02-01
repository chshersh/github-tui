let in_between ~sep list =
  let rec loop = function
    | [] -> []
    | x :: xs -> sep :: x :: loop xs
  in
  match list with
  | [] | [ _ ] -> list
  | x :: xs -> x :: loop xs

let of_sub_array ~offset ~len arr =
  let len =
    if offset + len > Array.length arr then Array.length arr - offset else len
  in
  let rec loop i = if i >= len then [] else arr.(offset + i) :: loop (i + 1) in
  loop 0

let max_on f list = List.fold_left (fun acc x -> max acc (f x)) 0 list

let map_with_fold ~f ~init =
  let[@tail_mod_cons] rec go acc = function
    | [] -> []
    | hd :: tl ->
        let b, acc = f hd acc in
        b :: go acc tl
  in
  go init
