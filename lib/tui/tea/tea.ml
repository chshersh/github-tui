module Key = Key

type 'model t = {
  init : 'model;
  view : 'model -> string;
  update : Key.t -> 'model -> [ `Render of 'model | `Quit ];
  mutable buffer : Buffer.t;
}

let make ~init ~view ~update =
  let buffer = Buffer.create 0 in
  { init; view; update; buffer }

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

let output ~buffer:_ model_str =
  let len = String.length model_str in
  let new_buffer = Buffer.create len in
  Buffer.add_string new_buffer model_str;

  Tty.Terminal.clear ();
  let _ = Unix.write Unix.stdout (Buffer.to_bytes new_buffer) 0 len in

  new_buffer

let run ?log_file tea =
  let log = Log.log ~path:log_file in

  let rec loop model =
    log ~tag:"loop" ~msg:"Loop start";

    let model_str = tea.view model in
    let new_buffer = output ~buffer:tea.buffer model_str in
    tea.buffer <- new_buffer;

    let key = Key.read () in
    log ~tag:"key" ~msg:(Key.show_read key);

    let updated =
      match key with
      | `Read key -> tea.update key model
      | _ -> `Render model
    in
    match updated with
    | `Render model -> loop model
    | `Quit -> ()
  in

  let terminal_io = setup () in
  Fun.protect ~finally:(shutdown terminal_io) (fun () -> loop tea.init)
