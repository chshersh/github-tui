(* A simple pretty printing combinator library *)

type styles = ANSITerminal.style list

type doc =
  | Empty
  | Str of styles * string
  | Vertical of doc * doc
  | Horizontal of doc * doc

let str string = Str ([], string)
let fmt styles string = Str (styles, string)

let row = function 
  | [] -> Empty
  | hd :: tl -> List.fold_left (fun l r -> Horizontal (l, r)) hd tl

let col = function 
  | [] -> Empty
  | hd :: tl -> List.fold_left (fun l r -> Vertical (l, r)) hd tl

type chunk =
  {
    styles: styles;
    string: string;
  }

let fmt_chunk {styles; string} =
  ANSITerminal.sprintf styles "%s" string

let mk_padding_chunk n =
  let padding = String.make n ' ' in
  {styles = []; string = padding}

type line = 
  { 
    chunks: chunk list
  }

let fmt_line line =
  line.chunks
  |> List.map fmt_chunk
  |> String.concat "" 

let line_len line = 
  List.fold_left
    (fun acc {string; _} -> acc + String_extra.graphemes_len string)
    0
    line.chunks

let zip_lines (l: line list) (r: line list) =
  let max_len_l = List.map line_len l |> List.fold_left max 0 in

  let rec zip l r =
    match (l, r) with
    | (l, []) -> 
      l

    | ([], r) -> 
      (* Optimisation: Add extra chunk only if padding is needed *)
      if max_len_l > 0 then
        let padding_chunk = mk_padding_chunk max_len_l in
        List.map (fun line -> { chunks = padding_chunk :: line.chunks }) r
      else
        r

    | (hd_l :: tl_l, hd_r :: tl_r) ->
      let left_len = line_len hd_l in

      (* Optimisation: Combine chunks when left is already max len *)
      if left_len >= max_len_l then
        let new_line = { chunks = hd_l.chunks @ hd_r.chunks } in
        new_line :: zip tl_l tl_r
      else
        let padding_chunk = mk_padding_chunk (max_len_l - left_len) in
        let new_line = { chunks = hd_l.chunks @ [padding_chunk] @ hd_r.chunks } in
        new_line :: zip tl_l tl_r
  in

  zip l r

let rec render_to_lines = function
  | Empty ->
    []
  | Str (styles, string) ->
    [{ chunks = [{styles; string}] }]
  | Vertical (top, bottom) ->
    render_to_lines top @ render_to_lines bottom
  | Horizontal (left, right) ->
    zip_lines (render_to_lines left) (render_to_lines right)

let render doc = 
  doc
  |> render_to_lines
  |> List.map fmt_line
  |> String_extra.unlines