let proc cmd =
  Printf.eprintf "🐚  %s\n%!" cmd;
  match Unix.system cmd with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED return_code ->
      Error
        (Printf.sprintf "command '%s' failed with code %d" (String.escaped cmd)
           return_code)
  | Unix.WSIGNALED number | Unix.WSTOPPED number ->
      Error
        (Printf.sprintf "command '%s' was stopped by signal (%d)"
           (String.escaped cmd) number)

let collect_chan (channel : in_channel) : string =
  let rec loop acc =
    match input_line channel with
    | exception End_of_file -> acc
    | line -> loop (acc ^ line ^ "\n")
  in
  loop ""

let proc_stdout cmd =
  let env = Unix.environment () in
  let ((proc_stdout, _proc_stdin, _proc_stderr) as process) =
    Unix.open_process_full cmd env
  in
  let stdout_result = collect_chan proc_stdout in
  let _ = Unix.close_process_full process in
  String.trim stdout_result
