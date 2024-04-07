let style_repo = ANSITerminal.[ Bold; blue ]
let style_selected = ANSITerminal.[ Bold; green ]
let style_directory = ANSITerminal.[ Bold; magenta ]

let debug_section (model : Model.t) =
  let debug_info = Printf.sprintf "%dw x %dh" model.width model.height in
  Pretty.str debug_info

let tabs_section cur_tab =
  let open Pretty in
  let sep = vertical [ str " "; str " "; str "─" ] in
  horizontal
    [
      Widget.code_tab ~is_selected:(cur_tab = Model.Code);
      sep;
      Widget.issues_tab ~is_selected:(cur_tab = Model.Issues);
      sep;
      Widget.pull_requests_tab ~is_selected:(cur_tab = Model.PullRequests);
    ]

let code_section (code_tab : Model.code_tab) =
  let current_path_doc =
    Widget.pwd code_tab.root_dir_path (Fs.zipper_parents code_tab.fs)
  in
  let fs_doc = Widget.fs code_tab in
  Pretty.vertical [ current_path_doc; fs_doc ]

let tab_content_section (model : Model.t) =
  let tab_doc =
    match model.current_tab with
    | Code -> code_section model.code_tab
    | Issues | PullRequests -> Pretty.str ""
  in
  Pretty.(horizontal [ tab_doc; horizontal_fill; Widget.about_doc ])

let to_doc (model : Model.t) =
  let open Pretty in
  let empty = str "" in
  let debug = debug_section model in
  let repo = fmt style_repo model.repo in
  let tabs = tabs_section model.current_tab in
  let content = tab_content_section model in

  vertical [ horizontal [ repo; str " "; debug ]; empty; tabs; content; empty ]

let view (model : Model.t) = model |> to_doc |> Pretty.render ~width:model.width
