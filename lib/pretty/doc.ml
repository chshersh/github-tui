module Style = Style

type t =
  | Str of Style.t * string
  | Horizontal_fill of string
  | Vertical of t list
  | Horizontal of t list
  | With_size of (width:int -> height:int -> t)

let str string = Str (Style.none, string)
let fmt styles string = Str (styles, string)
let horizontal cols = Horizontal cols
let vertical rows = Vertical rows
let horizontal_fill filler = Horizontal_fill filler
let with_size f = With_size f

type prerender =
  | Rendered of Layout.t
  | Fill of string

let rec render ~width ~height = function
  | Str (styles, string) -> Layout.fmt styles string
  | Horizontal_fill filler -> horizontal_fill_to_layout ~width filler
  | Vertical rows -> vertical_to_layout ~width ~height rows
  | Horizontal cols -> horizontal_to_layout ~width ~height cols
  | With_size f -> render ~width ~height (f ~width ~height)

and horizontal_fill_to_layout ~width filler =
  filler |> Extra.String.repeat_txt width |> Layout.str

and vertical_to_layout ~width ~height rows =
  let render_row row height =
    (* While traversing vertically, width doesn't change but the height decreases. *)
    let row_layout = render ~width ~height row in
    let remaining_height = height - Layout.height row_layout in
    (row_layout, remaining_height)
  in
  rows |> Extra.List.map_with_fold ~f:render_row ~init:height |> Layout.vertical

and horizontal_to_layout ~width ~height cols =
  (* Step [prerender]. Check if there's Horizontal_fill, render everything else
     and calculate rendered size. *)
  let prerender col size_taken =
    match col with
    | Horizontal_fill filler -> (Fill filler, size_taken)
    | other ->
        (* WARNING: The leftmost horizontal fill will consume all the remaining width *)
        let remaining_width = width - size_taken in
        let layout = render ~width:remaining_width ~height other in
        (Rendered layout, size_taken + Layout.width layout)
  in
  let prerendered, size_taken =
    Extra.List.map_and_fold ~f:prerender ~init:0 cols
  in

  (* Step [fill_size]. Calculate the size of remaining fill *)
  let fill_width = width - size_taken in

  (* Step [combine]. Extract rendered layouts and fill the missing part. *)
  prerendered
  |> List.map (function
       | Rendered layout -> layout
       | Fill filler ->
           filler |> Extra.String.repeat_txt fill_width |> Layout.str)
  |> Layout.horizontal
