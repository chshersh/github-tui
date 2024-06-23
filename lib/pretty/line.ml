type styles = ANSITerminal.style list

type chunk = {
  styles : styles;
  string : string;
}

let fmt_chunk { styles; string } = ANSITerminal.sprintf styles "%s" string

let replicate_chunk width s =
  if width <= 0 then { styles = []; string = "" }
  else
    let filling = Extra.String.repeat_txt width s in
    { styles = []; string = filling }

type t = {
  chunks : chunk list;
  length : int;
}

let length line = line.length

let of_chunks chunks =
  let length =
    List.fold_left
      (fun acc { string; _ } -> acc + Extra.String.width string)
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

let fmt line = line.chunks |> List.map fmt_chunk |> String.concat ""
