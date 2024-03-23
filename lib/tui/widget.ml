let style_selected = ANSITerminal.[ Bold; green ]

(* let style_directory = ANSITerminal.[ Bold; magenta ] *)

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
