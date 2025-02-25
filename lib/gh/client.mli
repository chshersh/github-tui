(** A low level function to perform a query. Reads $GITHUB_TOKEN from the
    environment.

    Usage example:

    {[
      let () =
        print_endline
        @@ Github_tui.Gh.query
             {|
      query {
        repository(owner: "chshersh", name: "github-tui") {
          issues(last: 2, states: [OPEN], orderBy: {field: CREATED_AT, direction: ASC}) {
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
    ]} *)

type error =
  | No_github_token
  | Bad_credentials of {
      msg : string;
      doc_url : string;
      code : int;
    }
  | Curl_error of {
      code : int;
      msg : string;
    }

val query : string -> (Yojson.Basic.t, error) result

(** Parse the response from the above query **)
val parse_response :
  string list -> (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a list
