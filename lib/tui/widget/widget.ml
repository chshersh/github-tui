module Code = Code
module Issue = Issue
module Pr = Pr
module Generic = Generic

let about_doc (model: Model.t) =
  let widget =
    [
      "┌─────╼ About ╾─────┐";
      "│ GitHub TUI v0.1.0 │";
      "│ :h - help         │";
      Printf.sprintf "│ Size: %dw x %dh  │" model.width model.height;
      "│ Moving around:    │";
      "│   ←↓↑→            │";
      "│   hjkl            │";
      "└───────────────────┘";
    ]
  in
  let open Pretty.Doc in
  widget |> List.map (fmt ANSITerminal.[ cyan ]) |> vertical
  [@@ocamlformat "disable"]

let tab_doc ~is_selected tab_lines =
  let open Pretty.Doc in
  let format = if is_selected then fmt Style.selected else str in
  tab_lines |> List.map format |> vertical

let code_tab ~is_selected =
  tab_doc ~is_selected
    [ "╭───────╮";
      "│ Code¹ │";
      "└───────┴";
    ] [@@ocamlformat "disable"]

let issues_tab ~is_selected =
  tab_doc ~is_selected
    [ "╭─────────╮";
      "│ Issues² │";
      "┴─────────┴";
    ] [@@ocamlformat "disable"]

let pull_requests_tab ~is_selected =
  tab_doc ~is_selected
    [ "╭────────────────╮";
      "│ Pull Requests³ │";
      "┴────────────────┴";
    ] [@@ocamlformat "disable"]
