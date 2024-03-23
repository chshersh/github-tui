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

let pwd_char = "\u{f413}"
let dir_char = "\u{f4d4}"
let empty_dir_char = "\u{f413}"
let file_char = "\u{f4a5}"

let pwd root_path parents =
  let nested_path =
    List.fold_left
      (fun acc cur -> Filename.concat acc cur)
      "" (List.rev parents)
  in
  let full_path = pwd_char ^ " " ^ Filename.concat root_path nested_path in
  Pretty.fmt style_directory full_path

(* Extra padding for:

   * 1: Unicode box character (left)
   * 1: Space after left box character
   * 1: File/Directory icon
   * 1: Space after icon
   * 1: Space before right box character
   * 1: Unicode box character (right)
*)
let file_name_padding = 6

let max_file_name_len files =
  files
  |> Array.map (fun file -> file |> Fs.file_name |> String_extra.graphemes_len)
  |> Array.fold_left max 0

let fmt_file ~max_name_len (tree : Fs.tree) =
  let pad = String_extra.fill_right max_name_len in
  match tree with
  | File name -> file_char ^ " " ^ pad name
  | Dir (name, [||]) -> empty_dir_char ^ " " ^ pad name
  | Dir (name, _) -> dir_char ^ " " ^ pad name

let current_level_to_doc (cursor : Fs.cursor) has_next =
  let open Pretty in

  let max_name_len = max_file_name_len cursor.files in
  let max_len = max_name_len + file_name_padding in

  (* Frame *)
  let top = "╭" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "├" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "╰" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╯" in

  (* Line *)
  let fmt_selected_name file = "│ " ^ fmt_file ~max_name_len file ^ " ├" in
  let fmt_name file = "│ " ^ fmt_file ~max_name_len file ^ " │" in
  let hi_pos = (2 * cursor.pos) + 1 in

  (* Combine *)
  cursor.files
  |> Array.to_list
  |> List.mapi (fun i file ->
         if i = cursor.pos && has_next then fmt_selected_name file
         else fmt_name file)
  |> List_extra.in_between ~sep:mid
  |> (fun lines -> [ top ] @ lines @ [ bot ])
  |> List.mapi (fun i s ->
         if i = hi_pos - 1 || i = hi_pos || i = hi_pos + 1 then
           fmt style_selected s
         else str s)
  |> col

let children_to_doc ~prev_total ~pos children =
  let open Pretty in

  let max_name_len = max_file_name_len children in
  let max_len = max_name_len + file_name_padding in

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

  (* Formatting single file name *)
  let fmt_name i file =
    let is_first_pos = i = 0 in
    let is_last_pos = i = Array.length children - 1 in
    let has_more_than_one = Array.length children > 1 in
    let prefix =
      if is_first_pos then if has_more_than_one then "┬" else "─"
      else if is_last_pos then "└"
      else "├"
    in
    prefix ^ "─┤ " ^ fmt_file ~max_name_len file ^ " │"
  in

  (* Next level files *)
  let files_doc =
    children
    |> Array.to_list
    |> List.mapi fmt_name
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
