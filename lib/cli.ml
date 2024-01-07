open Core

let repo_param =
  let open Command.Param in
  anon ("owner/repo" %: string)

let command =
  Command.basic
    ~summary:"Display GitHub TUI for the selected repository"
    (Command.Param.map repo_param ~f:(fun _repo () -> Tui.start ()))