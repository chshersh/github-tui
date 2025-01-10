module Key = Key

type 'model t = {
  init : 'model;
  view : 'model -> string;
  update : Key.t -> 'model -> [ `Render of 'model | `No_diff | `Quit ];
}

let make ~init ~view ~update = { init; view; update }

let setup () =
  let terminal_io = Tty.Stdin.setup () in
  Tty.Terminal.enter_alt_screen ();
  Tty.Escape_seq.hide_cursor_seq ();
  Tty.Terminal.clear ();
  terminal_io

let shutdown terminal_io () =
  Tty.Terminal.exit_alt_screen ();
  Tty.Escape_seq.show_cursor_seq ();
  Tty.Stdin.shutdown terminal_io

let output model_str =
  Tty.Terminal.clear ();
  print_endline model_str

let run ?log_file { init; view; update } =
  let log = Log.log ~path:log_file in
  let rec loop model =
    log ~tag:"loop" ~msg:"Loop start";

    let model_str = view model in
    output model_str;

    let key = Key.read () in
    log ~tag:"key" ~msg:(Key.show_read key);

    let updated =
      match key with
      | `Read key -> update key model
      | _ -> `No_diff
    in
    match updated with
    | `No_diff -> loop model
    | `Render model -> loop model
    | `Quit -> ()
  in

  let terminal_io = setup () in
  Fun.protect ~finally:(shutdown terminal_io) (fun () -> loop init)
