let style_repo =
  ANSITerminal.([Bold; blue])

let style_selected =
  ANSITerminal.([Bold; green])

let tabs_section cur_tab =
  let open Pretty in
  let p_tab tab txt = 
    if cur_tab = tab
    then fmt style_selected txt
    else str txt
  in
  let sep = col
    [
      str " ";
      str " ";
      str "─";
    ]
  in
  render @@ row
    [ col
        [
          p_tab Model.Code "╭──────╮";
          p_tab Model.Code "│ Code │";
          p_tab Model.Code "└──────┴";
        ];
      sep;
      col
        [
          p_tab Model.Issues "╭────────╮";
          p_tab Model.Issues "│ Issues │";
          p_tab Model.Issues "┴────────┴";
        ];
      sep;
      col
        [
          p_tab Model.PullRequests "╭───────────────╮";
          p_tab Model.PullRequests "│ Pull Requests │";
          p_tab Model.PullRequests "┴───────────────┴";
        ];
    ]

let file_widget ~max_name_len ~selected files =
  (* Add two spaces for padding before and end of the file name *)
  let max_len = max_name_len + 4 in
  let top = "╭" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "├" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "╰" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╯" in
  let fmt_line line = 
    "│ " ^ String_extra.fill_right max_name_len line ^ " │"
  in
  let hi_pos = 2 * selected + 1 in
  files
  |> Array.to_list
  |> List.map fmt_line
  |> List_extra.in_between ~sep:mid
  |> (fun lines -> [top] @ lines @ [bot])
  |> List.mapi (fun i s ->
    if i = hi_pos - 1 || i = hi_pos || i = hi_pos + 1
      then ANSITerminal.sprintf style_selected "%s" s
      else s
  )
  |> String_extra.unlines

let code_section (code_tab: Model.code_tab) =
  let cursor = code_tab.fs.current in
  let files = Array.map Fs.file_name cursor.files in
  let max_name_len = 
    files 
    |> Array.map String_extra.graphemes_len
    |> Array.fold_left max 0 in
  file_widget ~max_name_len ~selected:cursor.pos files

let tab_content_section (model: Model.t) =
  match model.current_tab with
  | Code -> code_section model.code_tab
  | Issues | PullRequests -> ""

let view (model: Model.t) =
  let repo = ANSITerminal.sprintf style_repo "%s" model.repo in
  let tabs = tabs_section model.current_tab in
  let content = tab_content_section model in
  Format.sprintf 
{|%s

%s

%s

|} repo tabs content