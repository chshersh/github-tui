open Minttea

let move_fs move_fn (code_tab : Model.code_tab) =
  let fs = move_fn code_tab.fs in
  { code_tab with fs }

let move_up (model : Model.t) =
  match model.current_tab with
  | Code -> { model with code_tab = move_fs Fs.go_up model.code_tab }
  | Issues | PullRequests -> model

let move_down (model : Model.t) =
  match model.current_tab with
  | Code -> { model with code_tab = move_fs Fs.go_down model.code_tab }
  | Issues | PullRequests -> model

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
  | Event.KeyDown ((Key "q" | Escape), _modifier) ->
      (model, Command.Seq [ Command.Exit_alt_screen; Command.Quit ])
  (* if we press a digit, we switch to the corresponding tab *)
  | Event.KeyDown (Key "1", _modifier) ->
      ({ model with current_tab = Model.Code }, Command.Noop)
  | Event.KeyDown (Key "2", _modifier) ->
      ({ model with current_tab = Model.Issues }, Command.Noop)
  | Event.KeyDown (Key "3", _modifier) ->
      ({ model with current_tab = Model.PullRequests }, Command.Noop)
  (* directions/movements *)
  | Event.KeyDown ((Up | Key "k"), _modifier) -> (move_up model, Command.Noop)
  | Event.KeyDown ((Down | Key "j"), _modifier) ->
      (move_down model, Command.Noop)
  | Event.KeyDown ((Left | Key "h"), _modifier) ->
      (move_back model, Command.Noop)
  | Event.KeyDown ((Right | Key "l"), _modifier) ->
      (move_next model, Command.Noop)
  (* otherwise, we do nothing *)
  | _ -> (model, Command.Noop)
