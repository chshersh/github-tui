let fmt (styles : ANSITerminal.style list) : string -> string =
  ANSITerminal.sprintf styles "%s"

let fmt_repo =
  fmt ANSITerminal.([Bold; green])

let view (model: Model.t) =
  let repo = fmt_repo model.repo in
  let tab = 
    match model.tab with
    | Code -> "Code"
    | Issues -> "Issues"
    | PullRequests -> "Pull Requests" 
  in
  Format.sprintf 
{|%s

%s

|} repo tab