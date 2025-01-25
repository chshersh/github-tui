module Doc = Pretty.Doc
module Style = Doc.Style
module Layout = Pretty.Layout

type item = {
  is_selected : bool;
  item_type : item_type;
}

and item_type =
  | Separator of string
  | Lines of string list

let of_sep sep = { is_selected = false; item_type = Separator sep }
let of_lines lines = { is_selected = false; item_type = Lines lines }

(* Extra padding for:

   * 1: Unicode box character (left)
   * 1: Space after left box character
   * 1: Space before right box character
   * 1: Unicode box character (right)
*)
let item_padding = 4

let vlist_border ~selected items =
  let max_item_width = Extra.List.max_on Layout.width items in
  let pad = Extra.String.fill_right max_item_width in
  let max_width = max_item_width + item_padding in

  (* Frame *)
  let top = of_sep @@ "╭" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "╮" in
  let mid = of_sep @@ "├" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "┤" in
  let bot = of_sep @@ "╰" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "╯" in

  (* Selected index in the border list *)
  let highlight_index = (2 * selected) + 1 in

  (* Line *)
  let fmt_line line = Doc.str ("│ " ^ line ^ " │") in
  let fmt_selected_line line =
    Doc.(
      horizontal [ fmt Style.selected "│ "; str line; fmt Style.selected " │" ])
  in
  let to_lines_item item =
    item |> Layout.to_lines |> List.map pad |> of_lines
  in

  let render_item { is_selected; item_type } =
    match item_type with
    | Separator sep ->
        if is_selected then Doc.[ fmt Style.selected sep ] else Doc.[ str sep ]
    | Lines lines ->
        if is_selected then List.map fmt_selected_line lines
        else List.map fmt_line lines
  in

  (* Combine *)
  items
  |> List.map to_lines_item
  |> Extra.List.in_between ~sep:mid
  |> (fun lines -> [ top ] @ lines @ [ bot ])
  |> List.mapi (fun i item ->
         if
           i = highlight_index - 1
           || i = highlight_index
           || i = highlight_index + 1
         then { item with is_selected = true }
         else item)
  |> List.concat_map render_item
  |> Doc.vertical
