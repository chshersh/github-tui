(*
    Copied and modified from the original implementation at:

    - https://github.com/ocaml-tui/tty/blob/d9fad1057a21961eb40564611545a1e0700bc7b3/tty/stdin.ml
*)
let stdin_fd = Unix.descr_of_in_channel stdin
let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual

let try_read () =
  let bytes = Bytes.create 8 in
  let ready, _, _ = Unix.select [ stdin_fd ] [] [] (-1.0) in
  if ready = [] then ()
  else
    match Unix.read stdin_fd bytes 0 8 with
    | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> ()
    | len -> Uutf.Manual.src decoder bytes 0 len

let uchar_to_str u =
  let buf = Buffer.create 8 in
  Uutf.Buffer.add_utf_8 buf u;
  Buffer.contents buf

let rec read_utf8 () =
  match Uutf.decode decoder with
  | `Uchar u -> `Read (uchar_to_str u)
  | `End -> `End
  | `Malformed err -> `Malformed err
  | `Await ->
      (* We read in a blocking way, so there always should be something in the recursive call. *)
      try_read ();
      read_utf8 ()
