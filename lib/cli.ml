open Cmdliner

let repo_arg =
  let doc = "The GitHub repository to view in TUI." in
  Arg.(value & pos 0 string "NOT_SPECIFIED" & info [] ~docv:"OWNER/REPO" ~doc)

let path_arg =
  let doc = "Path to a local directory of a GitHub repository" in
  Arg.(value & opt string "." & info ["d"; "directory"] ~docv:"DIRECTORY_PATH" ~doc)

let gh_tui_t = Term.(const Tui.start $ repo_arg $ path_arg)

let cmd =
  let doc = "TUI of a GitHub repository" in
  let man = [
    `S Manpage.s_bugs;
    `P "Submit bug reports at: https://github.com/chshersh/github-tui/issues" ]
  in
  let info = Cmd.info "gh-tui" ~version:"0.1.0" ~doc ~man in
  Cmd.v info gh_tui_t
