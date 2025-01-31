module Json = Yojson.Basic.Util

type t = {
  number : int;
  title : string;
  author : string;
  state : state;
  labels : label list;
}

and label = {
  name : string;
  color : string;
}

and state =
  | Closed
  | Open

let mk_issues_query ~owner ~repo =
  Printf.sprintf
    {|
query {
  repository(owner: "%s", name: "%s") {
    issues(first: 10, states: [OPEN, CLOSED], orderBy: {field: CREATED_AT, direction: DESC}) {
        nodes {
          number
          title
          author {
            login
          }
          state
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

(* https://docs.github.com/en/graphql/reference/enums#issuestate *)
let parse_issue_state json =
  match json |> Json.to_string with
  | "OPEN" -> Open
  | "CLOSED" -> Closed
  | s -> failwith ("Unknown issue state: " ^ s)

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
  "state": "OPEN",
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
  let state = Json.(json |> member "state" |> parse_issue_state) in
  let labels =
    Json.(json |> member "labels" |> member "nodes" |> convert_each parse_label)
  in
  { number; title; author; state; labels }

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
  |> Result.map (fun response ->
         Client.parse_response issue_json_path parse_issue response)
