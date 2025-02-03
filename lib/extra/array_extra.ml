let of_sub_array ~offset ~len arr =
  let len =
    if offset + len > Array.length arr then Array.length arr - offset else len
  in
  let rec loop i = if i >= len then [] else arr.(offset + i) :: loop (i + 1) in
  loop 0

let max_on f arr = Array.fold_left (fun acc x -> max acc (f x)) 0 arr
