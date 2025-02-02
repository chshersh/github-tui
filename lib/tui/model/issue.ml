type t = {
  all_issues : Gh.Issue.t Render.t list Lazy.t;
  filter : filter;
  issues : Gh.Issue.t Render.t list Lazy.t;
  offset : int;
  error : Gh.Client.error option Lazy.t;
}

and filter =
  | State of Gh.Issue.state
  | All

let filter_all = All
let filter_open = State Gh.Issue.Open
let filter_closed = State Gh.Issue.Closed

let filter_issues ~filter issues =
  match filter with
  | All -> issues
  | State state ->
      issues
      |> List.filter (fun (issue : Gh.Issue.t Render.t) ->
             issue.item.state = state)

let lazy_filter_issues ~filter issues =
  lazy (issues |> Lazy.force |> filter_issues ~filter)

let apply_filter filter t =
  let issues = lazy_filter_issues ~filter t.all_issues in
  let offset =
    min t.offset (issues |> Lazy.force |> List.length |> fun l -> l - 1)
  in
  { t with filter; issues; offset }

let make ~owner ~repo =
  let issues_and_errors =
    lazy
      (match Gh.Issue.issues ~owner ~repo with
      | Ok issues ->
          let rendered = Render.issues issues in
          (rendered, None)
      | Error err -> ([], Some err))
  in
  let all_issues = lazy (issues_and_errors |> Lazy.force |> fst) in
  let error = lazy (issues_and_errors |> Lazy.force |> snd) in
  let filter = filter_open in
  let issues = lazy_filter_issues ~filter all_issues in
  let offset = 0 in
  { all_issues; filter; issues; offset; error }
