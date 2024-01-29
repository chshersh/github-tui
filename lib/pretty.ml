(* A simple pretty printing combinator library *)

type doc =
  | Empty
  | Str of string
  | Vertical of doc * doc
  | Horizontal of doc * doc

let (---) t b = Vertical (t, b)
let (<|>) l r = Horizontal (l, r)

let zip_lines l r =
  let max_len_l = List.map String_extra.graphemes_len l |> List.fold_left max 0 in
  let padding = String.make max_len_l ' ' in
  let rec zip l r =
    match (l, r) with
    | (l, []) -> 
      l
    | ([], r) -> 
      List.map (fun s -> padding ^ s) r
    | (hd_l :: tl_l, hd_r :: tl_r) ->
      (String_extra.fill_right max_len_l hd_l ^ hd_r) :: zip tl_l tl_r
  in
  zip l r

let rec render_to_lines = function
  | Empty ->
    []
  | Str s ->
    [s]
  | Vertical (top, bottom) ->
    render_to_lines top @ render_to_lines bottom
  | Horizontal (left, right) ->
    zip_lines (render_to_lines left) (render_to_lines right)

let render doc = 
  doc
  |> render_to_lines
  |> String_extra.unlines