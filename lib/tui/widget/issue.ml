let issue_char = "\u{f41b}"

let apply_background_color ~color text =
  let module Color = Pretty.Color in
  let color = Color.of_hex color in
  let foreground_suggestion = Color.foreground color in
  let color_code = Pretty.Color.to_escape_seq color in
  let text = Printf.sprintf "\027[48;%sm %s \027[0m" color_code text in
  (text, foreground_suggestion)

let section (issues_tab : Model.issues_tab) =
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
  |> Generic.vlist_border ~selected:issues_tab.offset
