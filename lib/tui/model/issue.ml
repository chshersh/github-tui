type t = {
  all_issues : Gh.Issue.t list Lazy.t;
  filter : filter;
  issues : Gh.Issue.t list Lazy.t;
  offset : int;
  error : Gh.Client.error option;
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
      issues |> List.filter (fun (issue : Gh.Issue.t) -> issue.state = state)

let lazy_filter_issues ~filter issues =
  lazy (issues |> Lazy.force |> filter_issues ~filter)

let apply_filter filter t =
  let issues = lazy_filter_issues ~filter t.all_issues in
  let offset =
    min t.offset (issues |> Lazy.force |> List.length |> fun l -> l - 1)
  in
  { t with filter; issues; offset }

let make ~owner ~repo =
  let all_issues, error =
    match Gh.Issue.issues ~owner ~repo with
    | Ok issues -> (lazy issues, None)
    | Error err -> (lazy [], Some err)
  in
  let filter = filter_open in
  let issues = lazy_filter_issues ~filter all_issues in
  let offset = 0 in
  { all_issues; filter; issues; offset; error }
