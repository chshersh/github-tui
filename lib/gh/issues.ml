module Json = Yojson.Basic.Util

type t = {
  number : int;
  title : string;
  author : string;
}

let mk_issues_query ~owner ~repo =
  Printf.sprintf
    {|
query {
  repository(owner: "%s", name: "%s") {
    issues(first: 2, states: [OPEN], orderBy: {field: CREATED_AT, direction: DESC}) {
        nodes {
          number
          title
          author {
            login
          }
        }
    }
  }
}
|}
    owner repo

(* Parse single issue from JSON:

{
  "number": 18,
  "title": "GitHub TUI: Release Tracker for v1.0",
  "author": {
    "login": "chshersh"
  }
}
*)
let parse_issue json =
  let number = Json.(member "number" json |> to_int) in
  let title = Json.(member "title" json |> to_string) in
  let author = Json.(member "author" json |> member "login" |> to_string) in
  { number; title; author }

(* Parse a list of 't' from the following JSON:

{
  "data": {
    "repository": {
      "issues": {
        "nodes": [
          {
            "number": 18,
            "title": "GitHub TUI: Release Tracker for v1.0",
            "author": {
              "login": "chshersh"
            }
          },
          {
            "number": 17,
            "title": "Refactor fs.ml: Separate concerns in go_down and go_up functions",
            "author": {
              "login": "chshersh"
            }
          }
        ]
      }
    }
  }
}
*)
let parse_issues json =
  json
  |> Yojson.Basic.from_string
  |> Json.path [ "data"; "repository"; "issues"; "nodes" ]
  |> Option.value ~default:(`List [])
  |> Json.convert_each parse_issue

let issues ~owner ~repo =
  mk_issues_query ~owner ~repo |> Client.query |> parse_issues
