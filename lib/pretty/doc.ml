module Style = Style

type t =
  | Str of Style.t * string
  | Horizontal_fill of string
  | Vertical of t list
  | Horizontal of t list

let str string = Str ([], string)
let fmt styles string = Str (styles, string)
let horizontal cols = Horizontal cols
let vertical rows = Vertical rows
let horizontal_fill filler = Horizontal_fill filler

type prerender =
  | Rendered of Layout.t
  | Fill of string

let ( += ) ref x = ref := !ref + x

let rec render ~width = function
  | Str (styles, string) -> Layout.fmt styles string
  | Horizontal_fill filler ->
      filler |> Extra.String.repeat_txt width |> Layout.str
  | Vertical rows -> rows |> List.map (render ~width) |> Layout.vertical
  | Horizontal cols -> horizontal_to_layout ~width cols

and horizontal_to_layout ~width cols =
  (* Step [prerender]. Check if there's Horizontal_fill, render everything else
     and calculate rendered size. *)
  let len = List.length cols in
  let size_taken = ref 0 in
  let prerendered = Array.make len (Fill "") in

  ListLabels.iteri cols ~f:(fun i col ->
      match col with
      | Horizontal_fill filler -> prerendered.(i) <- Fill filler
      | other ->
          (* WARNING: The leftmost horizontal fill will consume all the remaining width *)
          let remaining_width = width - !size_taken in
          let layout = render ~width:remaining_width other in
          size_taken += Layout.width layout;
          prerendered.(i) <- Rendered layout);

  (* Step [fill_size]. Calculate the size of remaining fill *)
  let fill_width = width - !size_taken in

  (* Step [combine]. Extract rendered layouts and fill the missing part. *)
  prerendered
  |> Array.to_list
  |> List.map (function
       | Rendered layout -> layout
       | Fill filler ->
           filler |> Extra.String.repeat_txt fill_width |> Layout.str)
  |> Layout.horizontal
