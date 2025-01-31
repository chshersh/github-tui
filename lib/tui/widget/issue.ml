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

let issue_char = "\u{f41b}"

let fmt_issue_state (state : Gh.Issue.state) =
  match state with
  | Open -> Layout.(fmt Style.issue_open issue_char)
  | Closed -> Layout.(fmt Style.issue_closed issue_char)

let fmt_title (issue : Gh.Issue.t) =
  let open Layout in
  horizontal
    [
      fmt_issue_state issue.state;
      str "  ";
      fmt Style.secondary (Printf.sprintf "#%d " issue.number);
      str issue.title;
      str " by ";
      fmt Style.bold (Printf.sprintf "@%s" issue.author);
    ]

let apply_background_color ~color text =
  let module Color = Pretty.Color in
  let color = Color.of_hex color in
  let foreground_suggestion = Color.foreground color in
  let color_code = Pretty.Color.to_escape_seq color in
  let text = Printf.sprintf "\027[48;%sm %s \027[0m" color_code text in
  (text, foreground_suggestion)

let fmt_labels labels =
  let open Layout in
  let fmt_label (label : Gh.Issue.label) =
    let label, foreground =
      apply_background_color ~color:label.color label.name
    in
    let label = label ^ " " in
    match foreground with
    | `Light -> fmt Style.fg_white label
    | `Dark -> fmt Style.fg_black label
  in
  labels |> List.map fmt_label |> fun labels -> horizontal (str "   " :: labels)

let fmt_issue (issue : Gh.Issue.t) =
  Layout.vertical [ fmt_title issue; fmt_labels issue.labels ]

let fmt_issues ~selected issues =
  issues |> Lazy.force |> List.map fmt_issue |> Generic.vlist_border ~selected

let section (issues_tab : Model.Issue.t) =
  let issues =
    match issues_tab.error with
    | None -> fmt_issues ~selected:issues_tab.offset issues_tab.issues
    | Some No_github_token -> Doc.str "\u{26A0} GITHUB_TOKEN not found"
  in
  Doc.(vertical [ fmt_filters issues_tab.filter; str ""; issues ])
