type styles = ANSITerminal.style list

type chunk = {
  styles : styles;
  string : string;
}

let fmt_chunk { styles; string } = ANSITerminal.sprintf styles "%s" string

let padding_chunk width =
  let padding = String.make width ' ' in
  { styles = []; string = padding }

type t = {
  chunks : chunk list;
  length : int;
}

let length line = line.length

let of_chunks chunks =
  let length =
    List.fold_left
      (fun acc { string; _ } -> acc + String_extra.graphemes_len string)
      0 chunks
  in
  { chunks; length }

let prepend_chunk chunk line =
  let chunks = chunk :: line.chunks in
  let length = String_extra.graphemes_len chunk.string + line.length in
  { chunks; length }

let append line1 line2 =
  let chunks = line1.chunks @ line2.chunks in
  let length = line1.length + line2.length in
  { chunks; length }

let fmt line = line.chunks |> List.map fmt_chunk |> String.concat ""
