let style_selected = ANSITerminal.[ Bold; green ]
let style_directory = ANSITerminal.[ Bold; magenta ]

let tab_doc ~is_selected tab_lines =
  let open Pretty in
  let format = if is_selected then fmt style_selected else str in
  tab_lines |> List.map format |> vertical

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

let pwd_char = "\u{e5fd}"
let dir_char = "\u{f4d4}"
let empty_dir_char = "\u{f413}"
let file_char = "\u{f4a5}"

let parents_path parents =
  List.fold_left (fun acc cur -> Filename.concat acc cur) "" (List.rev parents)

let pwd root_dir_path parents =
  let pwd_path = parents_path parents in
  let root_dir_name = Filename.basename root_dir_path in
  let full_path = pwd_char ^ " " ^ Filename.concat root_dir_name pwd_path in
  Pretty.fmt style_directory full_path

let file_contents_to_doc ~file_name:_ ~file_contents =
  let file_contents_preview =
    file_contents
    |> Lazy.force
    |> String.split_on_char '\n'
    |> List_extra.take 30
    |> List.map Pretty.str
    |> Pretty.vertical
  in
  Pretty.(horizontal [ str " "; file_contents_preview ])

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
  | File (name, _) -> file_char ^ " " ^ pad name
  | Dir (name, [||]) -> empty_dir_char ^ " " ^ pad name
  | Dir (name, _) -> dir_char ^ " " ^ pad name

let current_level_to_doc (cursor : Fs.cursor) ~has_next =
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
  |> vertical

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
    |> vertical
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
    |> vertical
  in

  horizontal [ connector_doc; files_doc ]

type next_level =
  | Empty_directory
  | Directory_contents of Pretty.doc
  | File_contents of Pretty.doc

let is_directory_contents = function
  | Directory_contents _ -> true
  | _ -> false

let next_level_to_doc ~prev_total ~pos (selected_file : Fs.tree) =
  (* Get the next level files *)
  match selected_file with
  (* No children of a file *)
  | File (file_name, file_contents) ->
      File_contents (file_contents_to_doc ~file_name ~file_contents)
  (* No children of a directory without children *)
  | Dir (_, [||]) -> Empty_directory
  (* Non-empty array of children *)
  | Dir (_, children) ->
      Directory_contents (children_to_doc ~prev_total ~pos children)

let fs (code_tab : Model.code_tab) =
  let fs = code_tab.fs in
  let current = fs.current in
  let next_level_doc =
    next_level_to_doc
      ~prev_total:(Array.length current.files)
      ~pos:current.pos (Fs.file_at current)
  in
  let current_level_doc =
    current_level_to_doc current
      ~has_next:(is_directory_contents next_level_doc)
  in
  match next_level_doc with
  | Empty_directory -> current_level_doc
  | Directory_contents next_level_doc | File_contents next_level_doc ->
      Pretty.horizontal [ current_level_doc; next_level_doc ]
