type doc =
  | Str of Line.styles * string
  | Horizontal_fill
  | Vertical of doc list
  | Horizontal of doc list

let str string = Str ([], string)
let fmt styles string = Str (styles, string)
let horizontal cols = Horizontal cols
let vertical rows = Vertical rows
let horizontal_fill = Horizontal_fill

let zip_lines (l : Line.t list) (r : Line.t list) =
  let max_len_l = List.map Line.length l |> List.fold_left max 0 in

  let rec zip l r =
    match (l, r) with
    | l, [] -> l
    | [], r ->
        (* Optimisation: Add extra chunk only if padding is needed *)
        if max_len_l > 0 then
          let padding_chunk = Line.padding_chunk max_len_l in
          List.map (Line.prepend_chunk padding_chunk) r
        else r
    | hd_l :: tl_l, hd_r :: tl_r ->
        let left_len = Line.length hd_l in

        (* Optimisation: Combine chunks when left is already max len *)
        if left_len >= max_len_l then
          let new_line = Line.append hd_l hd_r in
          new_line :: zip tl_l tl_r
        else
          let padding_chunk = Line.padding_chunk (max_len_l - left_len) in
          let new_line =
            Line.append hd_l (Line.append (Line.of_chunks [ padding_chunk ]) hd_r)
          in
          new_line :: zip tl_l tl_r
  in

  zip l r

let rec render_to_lines ~width = function
  | Str (styles, string) -> [ Line.of_chunks [ { styles; string } ] ]
  | Horizontal_fill -> [ Line.of_chunks [ Line.padding_chunk width ] ]
  | Vertical rows -> List.concat_map (render_to_lines ~width) rows
  | Horizontal cols -> (
      match cols with
      | [] -> []
      (* TODO: This is potentially really slow; optimise *)
      | hd :: tl ->
          List.fold_left
            (fun acc col -> zip_lines acc (render_to_lines ~width col))
            (render_to_lines ~width hd) tl)

let render ~width doc =
  doc |> render_to_lines ~width |> List.map Line.fmt |> String_extra.unlines
