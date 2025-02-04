module Style = Pretty.Style

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

let tab_content_section (model : Model.t) =
  let tab_doc =
    match model.current_tab with
    | Code -> Widget.Code.section model.code_tab
    | Issues -> Widget.Issue.section model.issues_tab
    | PullRequests -> Widget.Pr.section model.pull_requests_tab
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

let view (model : Model.t) =
  model |> to_doc |> Pretty.render ~width:model.width ~height:model.height
