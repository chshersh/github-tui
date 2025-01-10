(** This module provides a minimalistic TUI framework in The Elm Architecture
    (TEA) style. *)

module Key = Key

(** The main TUI type *)
type 'model t

(** Create a TUI app give the initial state, a function to render state and a
    function that handles updates.*)
val make :
  init:'model ->
  view:('model -> string) ->
  update:(Key.t -> 'model -> [ `Render of 'model | `No_diff | `Quit ]) ->
  'model t

(** Actually run the TUI application provided the setup *)
val run : ?log_file:string -> 'model t -> unit
