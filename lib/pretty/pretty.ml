type doc =
  | Str of Line.styles * string
  | Horizontal_fill of string
  | Vertical of doc list
  | Horizontal of doc list

let str string = Str ([], string)
let fmt styles string = Str (styles, string)
let horizontal cols = Horizontal cols
let vertical rows = Vertical rows
let horizontal_fill filler = Horizontal_fill filler

let zip_lines (l : Line.t list) (r : Line.t list) =
  let max_len_l = List.map Line.length l |> List.fold_left max 0 in

  let rec zip l r =
    match (l, r) with
    | l, [] -> l
    | [], r ->
        (* Optimisation: Add extra chunk only if padding is needed *)
        if max_len_l > 0 then
          let padding_chunk = Line.replicate_chunk max_len_l " " in
          List.map (Line.prepend_chunk padding_chunk) r
        else r
    | hd_l :: tl_l, hd_r :: tl_r ->
        let left_len = Line.length hd_l in

        (* Optimisation: Combine chunks when left is already max len *)
        if left_len >= max_len_l then
          let new_line = Line.append hd_l hd_r in
          new_line :: zip tl_l tl_r
        else
          let padding_chunk = Line.replicate_chunk (max_len_l - left_len) " " in
          let new_line =
            Line.append hd_l
              (Line.append (Line.of_chunks [ padding_chunk ]) hd_r)
          in
          new_line :: zip tl_l tl_r
  in

  zip l r

type prerender =
  | Rendered of Line.t list
  | Fill of string

let rec render_to_lines ~width = function
  | Str (styles, string) -> [ Line.of_chunks [ { styles; string } ] ]
  | Horizontal_fill filler ->
      [ Line.of_chunks [ Line.replicate_chunk width filler ] ]
  | Vertical rows -> List.concat_map (render_to_lines ~width) rows
  | Horizontal cols -> horizontal_to_lines ~width cols

and horizontal_to_lines ~width cols =
  (* Step [prerender]. Check if there's Horizontal_fill, render everything else
     and calculate rendered size. *)
  let size_taken, prerendered =
    List.fold_left
      (fun (size_taken, prerendered) col ->
        let new_size_taken, new_prerendered =
          match col with
          | Horizontal_fill filler -> (size_taken, Fill filler)
          | other ->
              (* WARNING: The leftmost horizontal fill will consume all the remaining width *)
              let remaining_width = width - size_taken in
              let rendered = render_to_lines ~width:remaining_width other in
              let max_line_width = Extra.List.max_on Line.length rendered in
              (size_taken + max_line_width, Rendered rendered)
        in
        (* TODO: Adding to the end of the list is suboptimal *)
        (new_size_taken, prerendered @ [ new_prerendered ]))
      (0, []) cols
  in

  (* Step [fill_size]. Calculate the size of remaining fill *)
  let fill_width = width - size_taken in

  (* Step [fill]. Fill the first Horizontal_fill *)
  let _, rendered =
    List.fold_left
      (fun (remaining_width, rendered) prerendered ->
        match prerendered with
        | Rendered lines -> (remaining_width, rendered @ [ lines ])
        | Fill filler ->
            ( 0,
              rendered
              @ [
                  [
                    Line.(of_chunks [ replicate_chunk remaining_width filler ]);
                  ];
                ] ))
      (fill_width, []) prerendered
  in

  (* Step [combine]. Combine the final columns of lines. *)
  match rendered with
  | [] -> []
  (* TODO: This is potentially really slow; optimise *)
  | hd :: tl -> List.fold_left zip_lines hd tl

let render ~width doc =
  doc |> render_to_lines ~width |> List.map Line.fmt |> Extra.String.unlines
