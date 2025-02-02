let in_between ~sep list =
  let rec loop = function
    | [] -> []
    | x :: xs -> sep :: x :: loop xs
  in
  match list with
  | [] | [ _ ] -> list
  | x :: xs -> x :: loop xs

let max_on f list = List.fold_left (fun acc x -> max acc (f x)) 0 list

let map_with_fold ~f ~init =
  let[@tail_mod_cons] rec go acc = function
    | [] -> []
    | hd :: tl ->
        let b, acc = f hd acc in
        b :: go acc tl
  in
  go init

let map_with_fold_acc ~f ~init =
  let[@tail_mod_cons] rec go acc rev_result = function
    | [] -> (List.rev rev_result, acc)
    | hd :: tl ->
        let b, acc = f hd acc in
        go acc (b :: rev_result) tl
  in
  go init []
