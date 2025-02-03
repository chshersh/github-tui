module Json = Yojson.Basic.Util

type error =
  | No_github_token
  | Bad_credentials
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
      | Ok { body; _ } ->
          let open Yojson.Basic in
          let json = from_string body in
          if Util.member "message" json = `String "Bad credentials" then
            Error Bad_credentials
          else Ok json)

let parse_response (path : string list) (parse_item : Yojson.Basic.t -> 'a) json
    =
  json
  |> Json.path path
  |> Option.value ~default:(`List [])
  |> Json.convert_each parse_item
