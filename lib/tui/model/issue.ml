type t = {
  all_issues : Gh.Issue.t Render.t list Lazy.t;
  filter : filter;
  issues : Gh.Issue.t Render.t array Lazy.t;
  offset : int;
  scroll_start : int;
  error : Gh.Client.error option Lazy.t;
}

and filter =
  | State of Gh.Issue.state
  | All

let filter_all = All
let filter_open = State Gh.Issue.Open
let filter_closed = State Gh.Issue.Closed

let filter_issues ~filter issues =
  let filtered_issues =
    match filter with
    | All -> issues
    | State state ->
        issues
        |> List.filter (fun (issue : Gh.Issue.t Render.t) ->
               issue.item.state = state)
  in
  Array.of_list filtered_issues

let lazy_filter_issues ~filter issues =
  lazy (issues |> Lazy.force |> filter_issues ~filter)

let issues_length issues = issues |> Lazy.force |> Array.length

let apply_filter filter t =
  let issues = lazy_filter_issues ~filter t.all_issues in
  let offset = min t.offset (issues |> issues_length |> fun l -> l - 1) in
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
  let scroll_start = 0 in
  { all_issues; filter; issues; offset; scroll_start; error }

(* Hardcoded number of maximum issues to display per scroll. *)
let max_issues = 10

let move direction tab =
  let offset = tab.offset + direction in
  let total_issues = issues_length tab.issues in
  let offset =
    if offset < 0 then 0
    else if offset >= total_issues then total_issues - 1
    else offset
  in
  let scroll_start =
    if tab.scroll_start < offset then offset
    else if tab.scroll_start + max_issues <= offset then
      tab.scroll_start + direction
    else tab.scroll_start
  in
  { tab with offset; scroll_start }
