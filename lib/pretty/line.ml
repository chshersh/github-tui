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
