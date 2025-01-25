module Doc = Pretty.Doc
module Layout = Pretty.Layout

(* Extra padding for:

   * 1: Unicode box character (left)
   * 1: Space after left box character
   * 1: Space before right box character
   * 1: Unicode box character (right)
*)
let item_padding = 4

let vlist_border items =
  let max_item_width = Extra.List.max_on Layout.width items in
  let pad = Extra.String.fill_right max_item_width in
  let max_width = max_item_width + item_padding in

  (* Frame *)
  let top = "╭" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "╮" in
  let mid = "├" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "┤" in
  let bot = "╰" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "╯" in

  (* Add border opening and close *)
  let wrap_lines lines = [ top ] @ lines @ [ bot ] in

  (* Line *)
  let fmt_item item =
    item
    |> Layout.to_lines
    |> List.map (fun item_line -> "│ " ^ pad item_line ^ " │")
  in

  (* Combine *)
  items
  |> List.map fmt_item
  |> Extra.List.in_between ~sep:[ mid ]
  |> List.concat
  |> wrap_lines
  |> List.map Doc.str
  |> Doc.vertical
