open Cmdliner

let owner_repo_arg =
  let doc = "The GitHub repository to view in TUI." in
  Arg.(value & pos 0 string "NOT_SPECIFIED" & info [] ~docv:"OWNER/REPO" ~doc)

let path_arg =
  let doc = "Path to a local directory of a GitHub repository" in
  Arg.(
    value
    & opt (some string) None
    & info [ "d"; "directory" ] ~docv:"DIRECTORY_PATH" ~doc)

let log_arg =
  let doc = "Log debug information to the given file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "l"; "log-to" ] ~docv:"LOG_PATH" ~doc)

let run owner_repo local_path log_file =
  Tui.start ~owner_repo ~local_path ~log_file

let gh_tui_term = Term.(const run $ owner_repo_arg $ path_arg $ log_arg)

let cmd =
  let doc = "TUI of a GitHub repository" in
  let man =
    [
      `S Manpage.s_bugs;
      `P "Submit bug reports at: https://github.com/chshersh/github-tui/issues";
    ]
  in
  let info = Cmd.info "gh-tui" ~version:"0.1.0" ~doc ~man in
  Cmd.v info gh_tui_term
