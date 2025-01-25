module Json = Yojson.Basic.Util

type label = {
  name : string;
  color : string;
}

type t = {
  number : int;
  title : string;
  author : string;
  labels : label list;
}

let mk_issues_query ~owner ~repo =
  Printf.sprintf
    {|
query {
  repository(owner: "%s", name: "%s") {
    issues(first: 10, states: [OPEN], orderBy: {field: CREATED_AT, direction: DESC}) {
        nodes {
          number
          title
          author {
            login
          }
          labels(first: 100) {
            nodes {
              name
              color
            }
          }
        }
    }
  }
}
|}
    owner repo

(* Parse single label from JSON:

{
  "name": "good first issue",
  "color": "7057ff"
}
*)
let parse_label json =
  let name = Json.(json |> member "name" |> to_string) in
  let color = Json.(json |> member "color" |> to_string) in
  { name; color }

(* Parse single issue from JSON:

{
  "number": 18,
  "title": "GitHub TUI: Release Tracker for v1.0",
  "author": {
    "login": "chshersh"
  },
  "labels": {
    "nodes": [
      {
        "name": "good first issue",
        "color": "7057ff"
      }
    ]
  }
}
*)
let parse_issue json =
  let number = Json.(json |> member "number" |> to_int) in
  let title = Json.(json |> member "title" |> to_string) in
  let author = Json.(json |> member "author" |> member "login" |> to_string) in
  let labels =
    Json.(json |> member "labels" |> member "nodes" |> convert_each parse_label)
  in
  { number; title; author; labels }

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
let issue_json_path = [ "data"; "repository"; "issues"; "nodes" ]

let issues ~owner ~repo =
  mk_issues_query ~owner ~repo
  |> Client.query
  |> Client.parse_response issue_json_path parse_issue
