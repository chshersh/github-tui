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
            Line.append hd_l
              (Line.append (Line.of_chunks [ padding_chunk ]) hd_r)
          in
          new_line :: zip tl_l tl_r
  in

  zip l r

type prerender_pass_info = {
  has_fill : bool;
  size_taken : int;
}

type prerender =
  | Rendered of Line.t list
  | Fill

let rec render_to_lines ~width = function
  | Str (styles, string) -> [ Line.of_chunks [ { styles; string } ] ]
  | Horizontal_fill -> [ Line.of_chunks [ Line.padding_chunk width ] ]
  | Vertical rows -> List.concat_map (render_to_lines ~width) rows
  | Horizontal cols -> horizontal_to_lines ~width cols

and horizontal_to_lines ~width cols =
  (* Step [prerender]. Check if there's Horizontal_fill, render everything else
     and calculate rendered size. *)
  let pass_info, prerendered =
    List.fold_left
      (fun (pass_info, prerendered) col ->
        let new_pass_info, new_prerendered =
          match col with
          | Horizontal_fill ->
              ({ has_fill = true; size_taken = pass_info.size_taken }, Fill)
          | other ->
              (* WARNING: This will pass the total width to the left-most Horizontal_fill *)
              let rendered = render_to_lines ~width other in
              let max_line_width = List_extra.max_on Line.length rendered in
              ( {
                  has_fill = pass_info.has_fill;
                  size_taken = pass_info.size_taken + max_line_width;
                },
                Rendered rendered )
        in
        (* TODO: This is suboptimal *)
        (new_pass_info, prerendered @ [ new_prerendered ]))
      ({ has_fill = false; size_taken = 0 }, [])
      cols
  in

  (* Step [fill_size]. Calculate the size of remaining fill *)
  let fill_width = width - pass_info.size_taken in

  (* Step [fill]. Fill the first Horizontal_fill *)
  let _, rendered =
    List.fold_left
      (fun (remaining_width, rendered) prerendered ->
        match prerendered with
        | Rendered lines -> (remaining_width, rendered @ [ lines ])
        | Fill ->
            ( 0,
              rendered
              @ [ [ Line.(of_chunks [ padding_chunk remaining_width ]) ] ] ))
      (fill_width, []) prerendered
  in

  (* Step [combine]. Combine the final columns of lines. *)
  match rendered with
  | [] -> []
  (* TODO: This is potentially really slow; optimise *)
  | hd :: tl -> List.fold_left zip_lines hd tl

let render ~width doc =
  doc |> render_to_lines ~width |> List.map Line.fmt |> String_extra.unlines
