let scroll ~lines ~span ~offset =
  let height = 40 in
  let scroll = Scroll.make ~height ~span ~lines ~offset in
  match scroll with
  | None -> Pretty.Doc.str " "
  | Some scroll ->
      let sections = Scroll.to_sections scroll in
      let before = List.init sections.before (fun _ -> Pretty.Doc.str "░") in
      let scroll = List.init sections.scroll (fun _ -> Pretty.Doc.str "█") in
      let after = List.init sections.after (fun _ -> Pretty.Doc.str "░") in
      let scroll_bar = before @ scroll @ after in
      Pretty.Doc.vertical scroll_bar

let pwd_char = "\u{e5fd}"
let dir_char = "\u{f4d4}"
let empty_dir_char = "\u{f413}"
let file_char = "\u{f4a5}"
let bin_char = "\u{eae8}"

let parents_path parents =
  List.fold_left (fun acc cur -> Filename.concat acc cur) "" (List.rev parents)

let pwd root_dir_path (fs : Fs.zipper) =
  let parents = Fs.zipper_parents fs in
  let pwd_path = parents_path parents in
  let root_dir_name = Filename.basename root_dir_path in
  let full_path = pwd_char ^ " " ^ Filename.concat root_dir_name pwd_path in
  Pretty.Doc.(fmt Style.directory full_path)

let file_contents_to_doc ~(file_contents : Fs.Filec.t) =
  let lines = file_contents.lines in
  let len_lines = Array.length lines in
  let span = 40 in
  let offset = file_contents.offset in

  let contents_span = Extra.List.of_sub_array ~offset ~len:span lines in

  let scroll_doc = scroll ~lines:len_lines ~span ~offset in
  let contents_doc = Pretty.Doc.vertical contents_span in
  Pretty.Doc.(horizontal [ scroll_doc; str " "; contents_doc ])

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
  |> Array.map (fun file -> file |> Fs.file_name |> Extra.String.width)
  |> Array.fold_left max 0

let fmt_file ~max_name_len (tree : Fs.tree) =
  let pad = Extra.String.fill_right max_name_len in
  match tree with
  | File (name, _, file_type) -> (
      match Lazy.force file_type with
      | Fs.Filec.Text -> file_char ^ " " ^ pad name
      | Fs.Filec.Binary -> bin_char ^ " " ^ pad name)
  | Dir (name, (lazy children)) -> (
      match children with
      | [||] -> empty_dir_char ^ " " ^ pad name
      | _ -> dir_char ^ " " ^ pad name)

let current_level_to_doc (cursor : Fs.dir_cursor) ~has_next ~is_file_chosen =
  let open Pretty.Doc in
  let max_name_len = max_file_name_len cursor.files in
  let max_len = max_name_len + file_name_padding in

  (* Frame *)
  let top = "╭" ^ Extra.String.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "├" ^ Extra.String.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "╰" ^ Extra.String.repeat_txt (max_len - 2) "─" ^ "╯" in

  (* Line *)
  let fmt_selected_name file = "│ " ^ fmt_file ~max_name_len file ^ " ├" in
  let fmt_name file = "│ " ^ fmt_file ~max_name_len file ^ " │" in
  let hi_pos = (2 * cursor.pos) + 1 in

  let style = if is_file_chosen then Style.chosen else Style.selected in

  (* Combine *)
  cursor.files
  |> Array.to_list
  |> List.mapi (fun i file ->
         if i = cursor.pos && has_next then fmt_selected_name file
         else fmt_name file)
  |> Extra.List.in_between ~sep:mid
  |> (fun lines -> [ top ] @ lines @ [ bot ])
  |> List.mapi (fun i s ->
         if i = hi_pos - 1 || i = hi_pos || i = hi_pos + 1 then fmt style s
         else str s)
  |> vertical

let children_to_doc ~prev_total ~pos children =
  let open Pretty.Doc in
  let max_name_len = max_file_name_len children in
  let max_len = max_name_len + file_name_padding in

  (* Frame *)
  let top = "  ╭" ^ Extra.String.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "│ ├" ^ Extra.String.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "  ╰" ^ Extra.String.repeat_txt (max_len - 2) "─" ^ "╯" in

  (* Connector arrow *)
  let prev_rows_count = (2 * prev_total) + 1 in
  let connect_pos = (2 * pos) + 1 in
  let connector_doc =
    List.init prev_rows_count (fun i ->
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
    |> Extra.List.in_between ~sep:mid
    |> (fun lines -> [ top ] @ lines @ [ bot ])
    |> (fun lines ->
    let pad_before = List.init (max (connect_pos - 1) 0) (fun _ -> "") in
    pad_before @ lines)
    |> List.map str
    |> vertical
  in

  horizontal [ connector_doc; files_doc ]

type next_level =
  | Empty_directory
  | Directory_contents of Pretty.Doc.t
  | File_contents of Pretty.Doc.t

let is_directory_contents = function
  | Directory_contents _ -> true
  | _ -> false

type selected_node =
  | File_selected of Fs.Filec.t
  | Dir_selected of {
      prev_total : int;
      pos : int;
      children : Fs.tree array Lazy.t;
    }

let next_level_to_doc selected_node =
  (* Get the next level files *)
  match selected_node with
  | File_selected file_contents ->
      File_contents (file_contents_to_doc ~file_contents)
  | Dir_selected { children; prev_total; pos } -> (
      let children = Lazy.force children in
      match children with
      (* No children of a directory without children *)
      | [||] -> Empty_directory
      (* Non-empty array of children *)
      | _ -> Directory_contents (children_to_doc ~prev_total ~pos children))

type fs_view = {
  left : Fs.dir_cursor;
  right : selected_node;
  is_file_chosen : bool;
}

let fs_to_view (fs : Fs.zipper) =
  let is_file_chosen =
    match fs.current with
    | File_cursor _ -> true
    | Dir_cursor _ -> false
  in

  let left, right =
    match (fs.current, fs.parents) with
    | File_cursor _, [] ->
        failwith
          "Error during rendering! Impossible to have a file without a parent"
    | File_cursor contents, parent :: _ -> (parent, File_selected contents)
    | Dir_cursor cursor, _ -> (
        match Fs.file_at cursor with
        | File (_, contents, _) -> (cursor, File_selected (Lazy.force contents))
        | Dir (_, children) ->
            ( cursor,
              Dir_selected
                {
                  children;
                  prev_total = Array.length cursor.files;
                  pos = cursor.pos;
                } ))
  in

  { left; right; is_file_chosen }

let file_view (fs : Fs.zipper) =
  let view = fs_to_view fs in

  let next_level_doc = next_level_to_doc view.right in

  let current_level_doc =
    current_level_to_doc view.left
      ~has_next:(is_directory_contents next_level_doc)
      ~is_file_chosen:view.is_file_chosen
  in
  match next_level_doc with
  | Empty_directory -> current_level_doc
  | Directory_contents next_level_doc | File_contents next_level_doc ->
      Pretty.Doc.horizontal [ current_level_doc; next_level_doc ]

let section (code_tab : Model.code_tab) =
  let current_path_doc = pwd code_tab.root_dir_path code_tab.fs in
  let fs_doc = file_view code_tab.fs in
  Pretty.Doc.vertical [ current_path_doc; fs_doc ]
