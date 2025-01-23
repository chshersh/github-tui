type t = {
  chunks : Chunk.t list;
  length : int;
}

let length line = line.length

let of_chunks chunks =
  let length =
    List.fold_left
      (fun acc { Chunk.string; _ } -> acc + Extra.String.width string)
      0 chunks
  in
  { chunks; length }

let prepend_chunk chunk line =
  let chunks = chunk :: line.chunks in
  let length = Extra.String.width chunk.string + line.length in
  { chunks; length }

let append line1 line2 =
  let chunks = line1.chunks @ line2.chunks in
  let length = line1.length + line2.length in
  { chunks; length }

let fmt line = line.chunks |> List.map Chunk.fmt |> String.concat ""

let zip_lines l r =
  let max_len_l = List.map length l |> List.fold_left max 0 in

  let rec zip l r =
    match (l, r) with
    | l, [] -> l
    | [], r ->
        (* Optimisation: Add extra chunk only if padding is needed *)
        if max_len_l > 0 then
          let padding_chunk = Chunk.replicate max_len_l " " in
          List.map (prepend_chunk padding_chunk) r
        else r
    | hd_l :: tl_l, hd_r :: tl_r ->
        let left_len = length hd_l in

        (* Optimisation: Combine chunks when left is already max len *)
        if left_len >= max_len_l then
          let new_line = append hd_l hd_r in
          new_line :: zip tl_l tl_r
        else
          let padding_chunk = Chunk.replicate (max_len_l - left_len) " " in
          let new_line =
            append hd_l (append (of_chunks [ padding_chunk ]) hd_r)
          in
          new_line :: zip tl_l tl_r
  in

  zip l r
