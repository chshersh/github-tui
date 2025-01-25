let repo_header (model : Model.t) =
  let owner_repo = model.owner ^ "/" ^ model.repo in
  Pretty.Doc.(fmt Style.repo owner_repo)

let tabs_section cur_tab =
  let open Pretty.Doc in
  let sep = vertical [ str " "; str " "; str "─" ] in
  let line = vertical [ str " "; str " "; horizontal_fill "─" ] in
  horizontal
    [
      Widget.code_tab ~is_selected:(cur_tab = Model.Code);
      sep;
      Widget.issues_tab ~is_selected:(cur_tab = Model.Issues);
      sep;
      Widget.pull_requests_tab ~is_selected:(cur_tab = Model.PullRequests);
      line;
    ]

let code_section (code_tab : Model.code_tab) =
  let current_path_doc = Widget.pwd code_tab.root_dir_path code_tab.fs in
  let fs_doc = Widget.file_view code_tab.fs in
  Pretty.Doc.vertical [ current_path_doc; fs_doc ]

let issue_char = "\u{f41b}"

let apply_background_color ~color text =
  let module Color = Pretty.Color in
  let color = Color.of_hex color in
  let foreground_suggestion = Color.foreground color in
  let color_code = Pretty.Color.to_escape_seq color in
  let text = Printf.sprintf "\027[48;%sm %s \027[0m" color_code text in
  (text, foreground_suggestion)

let issues_section (issues_tab : Model.issues_tab) =
  let open Pretty.Layout in
  let fmt_title (issue : Gh.Issue.t) =
    horizontal
      [
        fmt Style.green issue_char;
        str "  ";
        fmt Style.secondary (Printf.sprintf "#%d " issue.number);
        str issue.title;
        str " by ";
        fmt Style.bold (Printf.sprintf "@%s" issue.author);
      ]
  in
  let fmt_labels labels =
    let fmt_label (label : Gh.Issue.label) =
      let label, foreground =
        apply_background_color ~color:label.color label.name
      in
      let label = label ^ " " in
      match foreground with
      | `Light -> fmt Style.fg_white label
      | `Dark -> fmt Style.fg_black label
    in
    labels |> List.map fmt_label |> fun labels ->
    horizontal (str "   " :: labels)
  in
  let fmt_issue (issue : Gh.Issue.t) =
    vertical [ fmt_title issue; fmt_labels issue.labels ]
  in
  issues_tab.issues
  |> Lazy.force
  |> List.map fmt_issue
  |> Widget.Generic.vlist_border

let tab_content_section (model : Model.t) =
  let tab_doc =
    match model.current_tab with
    | Code -> code_section model.code_tab
    | Issues -> issues_section model.issues_tab
    | PullRequests -> Pretty.Doc.str ""
  in
  tab_doc

let to_doc (model : Model.t) =
  let open Pretty.Doc in
  let empty = str "" in
  let about = Widget.about_doc model in
  let repo = repo_header model in
  let tabs = tabs_section model.current_tab in
  let content = tab_content_section model in

  vertical
    [
      repo;
      empty;
      tabs;
      horizontal [ content; horizontal_fill " "; about ];
      empty;
    ]

let view (model : Model.t) = model |> to_doc |> Pretty.render ~width:model.width
