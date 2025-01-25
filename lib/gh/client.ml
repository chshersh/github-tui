module Json = Yojson.Basic.Util

let github_api_url = "https://api.github.com/graphql"

let query query_body =
  let token = Sys.getenv "GITHUB_TOKEN" in

  let response =
    Ezcurl.post ~params:[]
      ~headers:
        [
          ("Authorization", "bearer " ^ token);
          ("User-Agent", "chshersh/github-tui");
        ]
      ~content:(`String (Printf.sprintf "{ \"query\": %S }" query_body))
      ~url:github_api_url ()
  in
  match response with
  | Error (_code, msg) -> Printf.sprintf "Error: %s" msg
  | Ok response -> response.body

let parse_response (path : string list) (parse_item : Yojson.Basic.t -> 'a) json
    =
  json
  |> Yojson.Basic.from_string
  |> Json.path path
  |> Option.value ~default:(`List [])
  |> Json.convert_each parse_item
