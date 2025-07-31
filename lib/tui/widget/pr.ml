module Style = Pretty.Style

let section (tab : Model.Pr.t) (icons : Pretty.Icon.t) =
  let open Pretty.Doc in
  let docs =
    match tab.error with
    | None ->
        let fmt_state = function
          | None -> str ""
          | Some Gh.Pr.Merged -> fmt Style.pr_merged icons.merged_char
          | Some Gh.Pr.Open -> fmt Style.pr_open icons.open_char
          | Some Gh.Pr.Closed -> fmt Style.pr_closed icons.closed_char
        in
        let fmt_pr (pr : Gh.Pr.t) =
          Pretty.Doc.(
            horizontal
              [
                fmt_state pr.state;
                str " ";
                fmt Style.secondary (Printf.sprintf "#%d " pr.number);
                str pr.title;
                str " by ";
                fmt Style.bold (Printf.sprintf "@%s" pr.author);
              ])
        in
        tab.pull_requests |> Lazy.force |> List.map fmt_pr
    | Some error -> Common.fmt_error error icons
  in
  vertical docs
