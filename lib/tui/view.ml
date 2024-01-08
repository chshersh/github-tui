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

let view (model: Model.t) =
  let repo = fmt_repo model.repo in
  let tabs = tab_section model.tab in
  Format.sprintf 
{|%s

%s

|} repo tabs