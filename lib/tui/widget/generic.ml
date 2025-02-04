module Doc = Pretty.Doc
module Style = Pretty.Style
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
let arrow_left_char = "\u{f0a8}"

(* Calculate the number of lines it takes to render [n] issues.

Calculations based on the following reasonin:

- Each issue takes 2 lines
- A border frame before each issue
- One last closing border at the end of all issues *)
let issues_height n = if n = 0 then 0 else (3 * n) + 1
let height = issues_height Model.Issue.max_issues
let span = height

let vlist_border ~scroll_start ~selected items =
  (* Create the scroll element *)
  let lines = items |> Array.length |> issues_height in
  let scroll = Scroll.make ~height ~span ~lines ~offset:(3 * scroll_start) in
  let scroll_doc =
    match scroll with
    | None -> Pretty.Doc.str " "
    | Some scroll -> scroll |> Scroll.to_sections |> Scroll.render
  in

  (* Calculations for frame padding *)
  let max_item_width = Extra.Array.max_on Layout.width items in
  let max_width = max_item_width + item_padding in

  (* Frame *)
  let top = of_sep @@ "╭" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "╮" in
  let mid = of_sep @@ "├" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "┤" in
  let bot = of_sep @@ "╰" ^ Extra.String.repeat_txt (max_width - 2) "─" ^ "╯" in

  (* Line *)
  let to_lines_item item = item |> Layout.to_lines |> of_lines in
  let fmt_line line = Doc.str ("│ " ^ line ^ " │") in
  let fmt_selected_line line =
    Doc.(
      horizontal [ fmt Style.selected "│ "; str line; fmt Style.selected " │" ])
  in
  let fmt_selected_lines = function
    | [] -> []
    | hd :: tl ->
        let first_line =
          Doc.(
            horizontal [ fmt_selected_line hd; str " "; str arrow_left_char ])
        in
        let other_lines = List.map fmt_selected_line tl in
        first_line :: other_lines
  in

  let render_item { is_selected; item_type } =
    match item_type with
    | Separator sep ->
        if is_selected then Doc.[ fmt Style.selected sep ] else Doc.[ str sep ]
    | Lines lines ->
        if is_selected then fmt_selected_lines lines
        else List.map fmt_line lines
  in

  (* We're working on elements between [scroll_start] and [scroll_start + max_issues].
     The invariant: [scroll_start <= selected].
     Therefore, we subtract [scroll_start] from [selected] to get the true
     offset in the resulting slice. *)
  let offset = selected - scroll_start in
  let highlight_index = (2 * offset) + 1 in

  (* Combine *)
  items
  |> Extra.Array.of_sub_array ~offset:scroll_start ~len:Model.Issue.max_issues
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
  |> fun issues -> Doc.horizontal [ scroll_doc; issues ]
