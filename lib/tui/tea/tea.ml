module Key = Key

type 'model t = {
  init : 'model;
  view : 'model -> string;
  update : Key.t -> 'model -> [ `Render of 'model | `Quit ];
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

let output_str = output_string stdout
let output_line str = output_str (str ^ "\n")

let clear_of width =
  let spaces = String.make width ' ' in
  output_line spaces

let output_diff ~old_line ~new_line =
  output_str new_line;

  let old_width = Extra.String.width old_line in
  let new_width = Extra.String.width new_line in

  if old_width <= new_width then output_line ""
  else
    let clear_width = old_width - new_width in
    clear_of clear_width

let rec output_lines ~old_lines ~new_lines =
  match (old_lines, new_lines) with
  | [], [] -> flush stdout
  | [], new_line :: new_lines ->
      output_line new_line;
      output_lines ~old_lines ~new_lines
  | old_line :: old_lines, [] ->
      clear_of (Extra.String.width old_line);
      output_lines ~old_lines ~new_lines
  | old_line :: old_lines, new_line :: new_lines ->
      output_diff ~old_line ~new_line;
      output_lines ~old_lines ~new_lines

let output ~old_lines model_str =
  let new_lines = String.split_on_char '\n' model_str in

  Tty.Terminal.move_cursor 0 0;
  output_lines ~old_lines ~new_lines;
  new_lines

let run ?path:_ tea =
  let rec loop old_lines model =
    let model_str = tea.view model in
    let new_lines = output ~old_lines model_str in

    let key = Key.read () in

    let updated =
      match key with
      | `Read key -> tea.update key model
      | _ -> `Render model
    in
    match updated with
    | `Render model -> loop new_lines model
    | `Quit -> ()
  in

  let terminal_io = setup () in
  Fun.protect ~finally:(shutdown terminal_io) (fun () -> loop [] tea.init)
