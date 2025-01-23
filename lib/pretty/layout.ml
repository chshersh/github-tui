module Style = Style

type t = {
  width : int;
  height : int;
  node : node;
}

and node =
  | Str of Style.t * string
  | Vertical of t list
  | Horizontal of t list

let width { width; _ } = width
let height { height; _ } = height

let str string =
  let width = Extra.String.width string in
  let height = 1 in
  let node = Str ([], string) in
  { width; height; node }

let fmt styles string =
  let width = Extra.String.width string in
  let height = 1 in
  let node = Str (styles, string) in
  { width; height; node }

let horizontal cols =
  let width =
    ListLabels.fold_left cols ~init:0 ~f:(fun acc t -> width t + acc)
  in
  let height = Extra.List.max_on height cols in
  let node = Horizontal cols in
  { width; height; node }

let vertical rows =
  let width = Extra.List.max_on width rows in
  let height =
    ListLabels.fold_left rows ~init:0 ~f:(fun acc t -> height t + acc)
  in
  let node = Vertical rows in
  { width; height; node }

let zip_lines (l : Line.t list) (r : Line.t list) =
  let max_len_l = List.map Line.length l |> List.fold_left max 0 in

  let rec zip l r =
    match (l, r) with
    | l, [] -> l
    | [], r ->
        (* Optimisation: Add extra chunk only if padding is needed *)
        if max_len_l > 0 then
          let padding_chunk = Chunk.replicate max_len_l " " in
          List.map (Line.prepend_chunk padding_chunk) r
        else r
    | hd_l :: tl_l, hd_r :: tl_r ->
        let left_len = Line.length hd_l in

        (* Optimisation: Combine chunks when left is already max len *)
        if left_len >= max_len_l then
          let new_line = Line.append hd_l hd_r in
          new_line :: zip tl_l tl_r
        else
          let padding_chunk = Chunk.replicate (max_len_l - left_len) " " in
          let new_line =
            Line.append hd_l
              (Line.append (Line.of_chunks [ padding_chunk ]) hd_r)
          in
          new_line :: zip tl_l tl_r
  in

  zip l r

let rec render_to_lines { width = _; height = _; node } =
  match node with
  | Str (styles, string) -> [ Line.of_chunks [ { styles; string } ] ]
  | Vertical rows -> List.concat_map render_to_lines rows
  | Horizontal cols -> horizontal_to_lines cols

and horizontal_to_lines cols =
  let horizontal_lines = List.map render_to_lines cols in
  match horizontal_lines with
  | [] -> []
  (* TODO: This has quadratic time complexity; optimise *)
  | hd :: tl -> List.fold_left zip_lines hd tl

let to_lines layout = layout |> render_to_lines |> List.map Line.fmt
