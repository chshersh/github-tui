let style_selected = ANSITerminal.[ Bold; green ]
let style_directory = ANSITerminal.[ Bold; magenta ]

let tab_doc ~is_selected tab_lines =
  let open Pretty in
  let format = if is_selected then fmt style_selected else str in
  tab_lines |> List.map format |> col

let code_tab ~is_selected =
  tab_doc ~is_selected
    [ "╭──────╮";
      "│ Code │";
      "└──────┴";
    ] [@@ocamlformat "disable"]

let issues_tab ~is_selected =
  tab_doc ~is_selected
    [ "╭────────╮";
      "│ Issues │";
      "┴────────┴";
    ] [@@ocamlformat "disable"]

let pull_requests_tab ~is_selected =
  tab_doc ~is_selected
    [ "╭───────────────╮";
      "│ Pull Requests │";
      "┴───────────────┘";
    ] [@@ocamlformat "disable"]

let pwd root_path parents =
  let nested_path =
    List.fold_left
      (fun acc cur -> Filename.concat acc cur)
      "" (List.rev parents)
  in
  let full_path = Filename.concat root_path nested_path in
  Pretty.fmt style_directory full_path

let current_level_to_doc (cursor : Fs.cursor) has_next =
  let open Pretty in
  let files = Array.map Fs.file_name cursor.files in
  let max_name_len =
    files |> Array.map String_extra.graphemes_len |> Array.fold_left max 0
  in

  (* Add two spaces for padding before and end of the file name *)
  let max_len = max_name_len + 4 in

  (* Frame *)
  let top = "╭" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "├" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "╰" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╯" in

  (* Line *)
  let fmt_selected_line line =
    "│ " ^ String_extra.fill_right max_name_len line ^ " ├"
  in
  let fmt_line line = "│ " ^ String_extra.fill_right max_name_len line ^ " │" in
  let hi_pos = (2 * cursor.pos) + 1 in

  (* Combine *)
  files
  |> Array.to_list
  |> List.mapi (fun i line ->
         if i = cursor.pos && has_next then fmt_selected_line line
         else fmt_line line)
  |> List_extra.in_between ~sep:mid
  |> (fun lines -> [ top ] @ lines @ [ bot ])
  |> List.mapi (fun i s ->
         if i = hi_pos - 1 || i = hi_pos || i = hi_pos + 1 then
           fmt style_selected s
         else str s)
  |> col

let children_to_doc ~prev_total ~pos children =
  let open Pretty in
  (* This array is guaranteed to be non-empty at this point *)
  let files = Array.map Fs.file_name children in
  let max_name_len =
    files |> Array.map String_extra.graphemes_len |> Array.fold_left max 0
  in

  (* Add two spaces for padding before and end of the file name *)
  let max_len = max_name_len + 4 in

  (* Frame *)
  let top = "  ╭" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "│ ├" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "  ╰" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╯" in

  (* Connector arrow *)
  let prev_rows_count = (2 * prev_total) + 1 in
  let connect_pos = (2 * pos) + 1 in
  let connector_doc =
    List_extra.generate prev_rows_count (fun i ->
        let is_current_pos = i = connect_pos in
        if is_current_pos then str "─" else str " ")
    |> col
  in

  (* Line *)
  let fmt_line i line =
    let is_first_pos = i = 0 in
    let is_last_pos = i = Array.length children - 1 in
    let has_more_than_one = Array.length children > 1 in
    let prefix =
      if is_first_pos then if has_more_than_one then "┬" else "─"
      else if is_last_pos then "└"
      else "├"
    in
    prefix ^ "─┤ " ^ String_extra.fill_right max_name_len line ^ " │"
  in

  (* Next level files *)
  let files_doc =
    files
    |> Array.to_list
    |> List.mapi fmt_line
    |> List_extra.in_between ~sep:mid
    |> (fun lines -> [ top ] @ lines @ [ bot ])
    |> (fun lines ->
         let pad_before =
           List_extra.generate (max (connect_pos - 1) 0) (fun _ -> "")
         in
         pad_before @ lines)
    |> List.map str
    |> col
  in

  row [ connector_doc; files_doc ]

let next_level_to_doc ~prev_total ~pos (selected_file : Fs.tree) =
  (* Get the next level files *)
  match selected_file with
  (* No children of a file *)
  | File _ -> None
  (* No children of a directory without children *)
  | Dir (_, [||]) -> None
  (* Non-empty array of children *)
  | Dir (_, children) -> Some (children_to_doc ~prev_total ~pos children)

let fs (fs : Fs.zipper) =
  let current = fs.current in
  let next_level_doc =
    Option.bind (Fs.file_at current)
      (next_level_to_doc
         ~prev_total:(Array.length current.files)
         ~pos:current.pos)
  in
  let current_level_doc =
    current_level_to_doc current (Option.is_some next_level_doc)
  in
  match next_level_doc with
  | None -> current_level_doc
  | Some next_level_doc -> Pretty.row [ current_level_doc; next_level_doc ]
