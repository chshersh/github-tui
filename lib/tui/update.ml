open Minttea

let move_up (model: Model.t) = match model.current_tab with
  | Code ->
    let fs = Fs.go_up model.code_tab.fs in
    let new_code_tab = { model.code_tab with fs = fs } in
    { model with code_tab = new_code_tab }
  | Issues | PullRequests -> model

let move_down (model: Model.t) = match model.current_tab with
  | Code ->
    let fs = Fs.go_down model.code_tab.fs in
    let new_code_tab = { model.code_tab with fs = fs } in
    { model with code_tab = new_code_tab }
  | Issues | PullRequests -> model

let move_left model = model
let move_right model = model

let update event (model: Model.t) =
  match event with
  (* if we press `q` or the escape key, we exit *)
  | Event.KeyDown (Key "q" | Escape) -> (model, Command.Seq [Command.Exit_alt_screen; Command.Quit])

  (* if we press a digit, we switch to the corresponding tab *)
  | Event.KeyDown (Key "1") ->
    ({ model with current_tab = Model.Code }, Command.Noop)
  | Event.KeyDown (Key "2") ->
    ({ model with current_tab = Model.Issues }, Command.Noop)
  | Event.KeyDown (Key "3") ->
    ({ model with current_tab = Model.PullRequests }, Command.Noop)

  (* directions/movements *)
  | Event.KeyDown (Up | Key "k") ->
    (move_up model, Command.Noop)
  | Event.KeyDown (Down | Key "j") ->
    (move_down model, Command.Noop)
  | Event.KeyDown (Left | Key "h") ->
    (move_left model, Command.Noop)
  | Event.KeyDown (Right | Key "l") ->
    (move_right model, Command.Noop)

  (* otherwise, we do nothing *)
  | _ -> (model, Command.Noop)
