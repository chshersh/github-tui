type t = {
  pull_requests : Gh.Pr.t list lazy_t;
  error : Gh.Client.error option;
}

let make ~owner ~repo =
  let pull_requests, error =
    match Gh.Pr.pull_requests ~owner ~repo with
    | Ok issues -> (lazy issues, None)
    | Error err -> (lazy [], Some err)
  in
  { pull_requests; error }
