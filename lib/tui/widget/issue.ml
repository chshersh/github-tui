module Doc = Pretty.Doc
module Style = Pretty.Style
module Layout = Pretty.Layout

let fmt_filter ~style ~is_selected filter_box =
  let fmt = if is_selected then Doc.fmt Style.(style & bold) else Doc.str in
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
    ~style:Style.none
    ~is_selected:(current_filter = Model.Issue.filter_all)
    [ "╭─────────╮";
      "│ All   a │";
      "╰─────────╯";
    ] [@@ocamlformat "disable"]
  in
  let pad = Doc.str " " in
  Doc.horizontal [ filter_open; pad; filter_closed; pad; filter_all ]

let fmt_issues ~scroll_start ~selected issues icons =
  issues
  |> Lazy.force
  |> Array.map (fun (rendered : _ Render.t) -> rendered.layout)
  |> fun i -> Generic.vlist_border ~scroll_start ~selected i icons

let section (issues_tab : Model.Issue.t) icons =
  let docs =
    match issues_tab.error |> Lazy.force with
    | None ->
        [
          fmt_filters issues_tab.filter;
          Doc.str "";
          fmt_issues ~scroll_start:issues_tab.scroll_start
            ~selected:issues_tab.offset issues_tab.issues icons;
        ]
    | Some error -> Common.fmt_error error icons
  in
  Doc.vertical docs
