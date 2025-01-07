(** A module to describe and handle possible keyboard events.

    Copied and modified from the original implementation at:

    - https://github.com/leostera/minttea/tree/aa5ef898f6ce4b2a5b359dd6f985cbe2a3583b70
*)

type t =
  | Up
  | Down
  | Left
  | Right
  | Space
  | Escape
  | Backspace
  | Enter
  | Key of string

(** Parse [t] from the standard sequence of characters. *)
val parse : string -> t

(** Read a key event from [stdin]. *)
val read : unit -> [> `Retry | `End | `Malformed of string | `Read of t ]
