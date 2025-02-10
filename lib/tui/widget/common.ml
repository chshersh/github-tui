let fmt_error (error : Gh.Client.error) =
  let open Pretty.Doc in
  match error with
  | No_github_token ->
      [
        str
          (Pretty.Icon.warning
         ^ " GITHUB_TOKEN not found. Make sure it's configured in your \
            environment.");
        str "";
        str "If you don't have a token, visit the following page to create one:";
        str "  • https://github.com/settings/tokens";
      ]
  | Bad_credentials { msg; doc_url; code } ->
      [
        str (Pretty.Icon.warning ^ Format.sprintf " [%d] %s" code msg);
        str "";
        str ("Documentation url: " ^ doc_url);
      ]
  | Curl_error { code; msg } ->
      [
        str
          (Pretty.Icon.warning
          ^ Format.sprintf " GitHub API returned error code: %d" code);
        str "";
        str msg;
      ]
