module Json = Yojson.Basic.Util

type error =
  | No_github_token
  | Curl_error of {
      code : int;
      msg : string;
    }

let github_api_url = "https://api.github.com/graphql"

let query query_body =
  match Sys.getenv_opt "GITHUB_TOKEN" with
  | None -> Error No_github_token
  | Some token -> (
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
      | Error (code, msg) -> Error (Curl_error { code = Curl.errno code; msg })
      | Ok response -> Ok response.body)

let parse_response (path : string list) (parse_item : Yojson.Basic.t -> 'a) json
    =
  json
  |> Yojson.Basic.from_string
  |> Json.path path
  |> Option.value ~default:(`List [])
  |> Json.convert_each parse_item
