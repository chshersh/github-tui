(** A module to read key events from keyboards in a blocking way.

    Copied and modified from the original implementation at:

    - https://github.com/ocaml-tui/tty/blob/d9fad1057a21961eb40564611545a1e0700bc7b3/tty/stdin.ml
*)

(** [read_utf8 ()] will do a blocking read and either return the next valid
    UTF-8 string available in [stdin] or immediately return. *)
val read_utf8 : unit -> [> `End | `Malformed of string | `Read of string ]
