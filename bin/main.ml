let () = print_endline @@ Github_tui.Gh.query
{| 
query {
  repository(owner: "chshersh", name: "zbg") {
    issues(last: 2, states: [OPEN], orderBy: {field: CREATED_AT, direction: DESC}) {
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
