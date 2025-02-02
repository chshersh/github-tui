let in_between ~sep list =
  let rec loop = function
    | [] -> []
    | x :: xs -> sep :: x :: loop xs
  in
  match list with
  | [] | [ _ ] -> list
  | x :: xs -> x :: loop xs

let max_on f list = List.fold_left (fun acc x -> max acc (f x)) 0 list

let map_and_fold ~f ~init lst =
  let acc_ref = ref init in
  let[@tail_mod_cons] rec go = function
    | [] -> []
    | hd :: tl ->
        let b, new_acc = f hd !acc_ref in
        acc_ref := new_acc;
        b :: go tl
  in
  let res = go lst in
  (res, !acc_ref)

let map_with_fold ~f ~init lst =
  let res, _ = map_and_fold ~f ~init lst in
  res
