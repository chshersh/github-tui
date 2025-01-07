module Key = Key

type 'model t = {
  init : 'model;
  view : 'model -> string;
  update : Key.t -> 'model -> 'model;
}

let run { init; view; update } =
  let rec loop model =
    let output = view model in
    print_endline output;

    let key = Key.read () in
    let model =
      match key with
      | `Read key -> update key model
      | _ -> model
    in
    loop model
  in

  let terminal_io = Tty.Stdin.setup () in
  Fun.protect
    ~finally:(fun () -> Tty.Stdin.shutdown terminal_io)
    (fun () -> loop init)
