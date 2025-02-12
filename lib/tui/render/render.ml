module Style = Pretty.Style
module Layout = Pretty.Layout

type 'a t = {
  item : 'a;
  layout : Pretty.Layout.t;
}

let fmt_issue_state (state : Gh.Issue.state) (icons : Pretty.Icon.t) =
  match state with
  | Open -> Layout.(fmt Style.issue_open icons.issue_char)
  | Closed -> Layout.(fmt Style.issue_closed icons.issue_char)

let fmt_title (issue : Gh.Issue.t) (icons : Pretty.Icon.t) =
  let open Layout in
  horizontal
    [
      fmt_issue_state issue.state icons;
      str "  ";
      fmt Style.secondary (Printf.sprintf "#%d " issue.number);
      str issue.title;
      str " by ";
      fmt Style.bold (Printf.sprintf "@%s" issue.author);
    ]

let fmt_labels labels =
  let open Layout in
  let fmt_label (label : Gh.Issue.label) =
    let label, foreground =
      Style.apply_background_hex_color ~hex:label.color label.name
    in
    fmt (Style.color_of_theme foreground) (label ^ " ")
  in
  labels |> List.map fmt_label |> fun labels -> horizontal (str "   " :: labels)

let fmt_issue (issue : Gh.Issue.t) (icons : Pretty.Icon.t) =
  Layout.vertical [ fmt_title issue icons; fmt_labels issue.labels ]

let issues issue_list icons =
  let layouts = List.map (fun issue -> fmt_issue issue icons) issue_list in
  let max_issue_width = Extra.List.max_on Layout.width layouts in
  ListLabels.map2 issue_list layouts ~f:(fun issue layout ->
      let width = Layout.width layout in
      let padding_len = max_issue_width - width in
      let padding = Layout.str (Extra.String.repeat_txt padding_len " ") in
      (* NOTE: Two [padding] elements assume an issue renders into two lines *)
      let layout =
        Layout.(horizontal [ layout; vertical [ padding; padding ] ])
      in
      { item = issue; layout })
