open Minttea

let update event (model: Model.t) =
  match event with
  (* if we press `q` or the escape key, we exit *)
  | Event.KeyDown (Key "q" | Escape) -> (model, Command.Quit)

  (* if we press a digit, we switch to the corresponding tab *)
  | Event.KeyDown (Key "1") ->
    ({ tab = Model.Code }, Command.Noop)
  | Event.KeyDown (Key "2") ->
    ({ tab = Model.Issues }, Command.Noop)
  | Event.KeyDown (Key "3") ->
    ({ tab = Model.PullRequests }, Command.Noop)

  (* otherwise, we do nothing *)
  | _ -> (model, Command.Noop)