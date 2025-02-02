module Doc = Pretty.Doc
module Style = Doc.Style
module Layout = Pretty.Layout

let fmt_filter ~style ~is_selected filter_box =
  let fmt = if is_selected then Doc.fmt (style @ Style.bold) else Doc.str in
  filter_box |> List.map fmt |> Doc.vertical

let fmt_filters current_filter =
  let filter_open =
    fmt_filter
    ~style:Style.issue_open
    ~is_selected:(current_filter = Model.Issue.filter_open)
    [ "╭──────────╮";
      "│ Open   o │";
      "╰──────────╯";
    ] [@@ocamlformat "disable"]
  in
  let filter_closed =
    fmt_filter
    ~style:Style.issue_closed
    ~is_selected:(current_filter = Model.Issue.filter_closed)
    [ "╭────────────╮";
      "│ Closed   c │";
      "╰────────────╯";
    ] [@@ocamlformat "disable"]
  in
  let filter_all =
    fmt_filter
    ~style:[]
    ~is_selected:(current_filter = Model.Issue.filter_all)
    [ "╭─────────╮";
      "│ All   a │";
      "╰─────────╯";
    ] [@@ocamlformat "disable"]
  in
  let pad = Doc.str " " in
  Doc.horizontal [ filter_open; pad; filter_closed; pad; filter_all ]

let fmt_issues ~selected issues =
  issues
  |> Lazy.force
  |> Array.to_list
  |> List.map (fun (rendered : _ Render.t) -> rendered.layout)
  |> Generic.vlist_border ~selected

let section (issues_tab : Model.Issue.t) =
  let docs =
    match issues_tab.error |> Lazy.force with
    | None ->
        [
          fmt_filters issues_tab.filter;
          Doc.str "";
          fmt_issues ~selected:issues_tab.offset issues_tab.issues;
        ]
    | Some error -> Common.fmt_error error
  in
  Doc.vertical docs
