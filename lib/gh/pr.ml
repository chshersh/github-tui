module Json = Yojson.Basic.Util

type pr_state =
  | Merged
  | Open
  | Closed

type t = {
  number : int;
  title : string;
  author : string;
  state : pr_state option;
}

let state_of_string = function
  | "MERGED" -> Some Merged
  | "OPEN" -> Some Open
  | "CLOSED" -> Some Closed
  | _ -> None

let mk_pr_query ~owner ~repo =
  Printf.sprintf
    {|
query {
  repository(owner: "%s", name: "%s") {
    pullRequests(first: 10, orderBy: {field: CREATED_AT, direction: DESC}) {
        nodes {
          number
          title
          author {
            login
          }
          state
        }
    }
  }
}
|}
    owner repo

(* Parse single item from JSON:
{
  "number": 18,
  "title": "GitHub TUI: Release Tracker for v1.0",
  "author": {
    "login": "chshersh"
  },
  "state": "MERGED"
}
*)
let parse_pr json =
  let number = Json.(member "number" json |> to_int) in
  let title = Json.(member "title" json |> to_string) in
  let author = Json.(member "author" json |> member "login" |> to_string) in
  let state = Json.(member "state" json |> to_string) in
  { number; title; author; state = state_of_string state }

(* Parse a list of 't' from the following JSON:
{
  "data": {
    "repository": {
      "pullRequests": {
        "nodes": [
          {
            "number": 22,
            "title": "Switch to setup-ocaml@v3",
            "author": {
              "login": "chshersh"
            },
            "state": "MERGED"
          },
          ...
*)
let json_path = [ "data"; "repository"; "pullRequests"; "nodes" ]

let pull_requests ~owner ~repo =
  mk_pr_query ~owner ~repo
  |> Client.query
  |> Result.map (fun response ->
         Client.parse_response json_path parse_pr response)
