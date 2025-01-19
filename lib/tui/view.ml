let repo_header (model : Model.t) =
  let owner_repo = model.owner ^ "/" ^ model.repo in
  Pretty.(fmt Style.repo owner_repo)

let tabs_section cur_tab =
  let open Pretty in
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
  Pretty.vertical [ current_path_doc; fs_doc ]

let issues_section (issues_tab : Model.issues_tab) =
  let fmt_issue (issue : Gh.Issue.t) =
    Printf.sprintf "#%d %s by %s" issue.number issue.title issue.author
    |> Pretty.str
  in
  issues_tab.issues |> Lazy.force |> List.map fmt_issue |> Pretty.vertical

let tab_content_section (model : Model.t) =
  let tab_doc =
    match model.current_tab with
    | Code -> code_section model.code_tab
    | Issues -> issues_section model.issues_tab
    | PullRequests -> Pretty.str ""
  in
  tab_doc

let to_doc (model : Model.t) =
  let open Pretty in
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
