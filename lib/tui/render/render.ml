module Layout = Pretty.Layout

type 'a t = {
  item : 'a;
  layout : Pretty.Layout.t;
}

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

let issues issue_list =
  let layouts = List.map fmt_issue issue_list in
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
