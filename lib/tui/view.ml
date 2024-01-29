let fmt (styles : ANSITerminal.style list) : string -> string =
  ANSITerminal.sprintf styles "%s"

let fmt_repo =
  fmt ANSITerminal.([Bold; blue])

let fmt_selected_tab =
  fmt ANSITerminal.([Bold; green])

let tab_section cur_tab =
  let p_tab tab txt = 
    if cur_tab = tab
    then Pretty.Str (fmt_selected_tab txt)
    else Pretty.Str txt
  in
  Pretty.(render (
      (p_tab Model.Code "╭──────╮" ---
       p_tab Model.Code "│ Code │" ---
       p_tab Model.Code "└──────┴"
      ) <|>
      (Str " " ---
       Str " " ---
       Str "─"
      ) <|>
      (p_tab Model.Issues "╭────────╮" ---
       p_tab Model.Issues "│ Issues │" ---
       p_tab Model.Issues "┴────────┴"
      ) <|>
      (Str " " ---
       Str " " ---
       Str "─"
      ) <|>
      (p_tab Model.PullRequests "╭───────────────╮" ---
       p_tab Model.PullRequests "│ Pull Requests │" ---
       p_tab Model.PullRequests "┴───────────────┴"
      )
    )
  )

let file_widget ~max_name_len files =
  (* Add two spaces for padding before and end of the file name *)
  let max_len = max_name_len + 4 in
  let top = "╭" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╮" in
  let mid = "├" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "┤" in
  let bot = "╰" ^ String_extra.repeat_txt (max_len - 2) "─" ^ "╯" in
  let fmt_line line = 
    "│ " ^ String_extra.fill_right max_name_len line ^ " │"
  in
  files
  |> List.map fmt_line
  |> List_extra.in_between ~sep:mid
  |> (fun lines -> [top] @ lines @ [bot])
  |> String_extra.unlines

let files_section files =
  let max_name_len = files |> List.map String_extra.graphemes_len |> List.fold_left max 0 in
  file_widget ~max_name_len files

let view (model: Model.t) =
  let repo = fmt_repo model.repo in
  let tabs = tab_section model.tab in
  let files = files_section model.files in
  Format.sprintf 
{|%s

%s

%s

|} repo tabs files