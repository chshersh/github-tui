(** This module provides a minimalistic TUI framework in The Elm Architecture
    (TEA) style. *)

module Key = Key

type 'model t = {
  init : 'model;
  view : 'model -> string;
  update : Key.t -> 'model -> 'model;
}

(** Actually run the TUI application provided the setup *)
val run : ?log_file:string -> 'model t -> unit
