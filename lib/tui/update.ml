open Tea.Key

let move_fs move_fn (code_tab : Model.code_tab) =
  let fs = move_fn code_tab.fs in
  { code_tab with fs }

let move_issues move (issues_tab : Model.issues_tab) =
  let len = issues_tab.issues |> Lazy.force |> List.length in
  let offset = (issues_tab.offset + move + len) mod len in
  { issues_tab with offset }

let move_up (model : Model.t) =
  match model.current_tab with
  | Code -> { model with code_tab = move_fs Fs.go_up model.code_tab }
  | Issues -> { model with issues_tab = move_issues (-1) model.issues_tab }
  | PullRequests -> model

let move_down (model : Model.t) =
  match model.current_tab with
  | Code -> { model with code_tab = move_fs Fs.go_down model.code_tab }
  | Issues -> { model with issues_tab = move_issues 1 model.issues_tab }
  | PullRequests -> model

let move_back (model : Model.t) =
  match model.current_tab with
  | Code -> { model with code_tab = move_fs Fs.go_back model.code_tab }
  | Issues | PullRequests -> model

let move_next (model : Model.t) =
  match model.current_tab with
  | Code -> { model with code_tab = move_fs Fs.go_next model.code_tab }
  | Issues | PullRequests -> model

let update event (model : Model.t) =
  match event with
  (* if we press `q` or the escape key, we exit *)
  | Key "q" | Escape -> `Quit
  (* if we press a digit, we switch to the corresponding tab *)
  | Key "1" -> `Render { model with current_tab = Model.Code }
  | Key "2" -> `Render { model with current_tab = Model.Issues }
  | Key "3" -> `Render { model with current_tab = Model.PullRequests }
  (* directions/movements *)
  | Up | Key "k" -> `Render (move_up model)
  | Down | Key "j" -> `Render (move_down model)
  | Left | Key "h" -> `Render (move_back model)
  | Right | Key "l" -> `Render (move_next model)
  (* otherwise, we do nothing *)
  | _ -> `Render model
