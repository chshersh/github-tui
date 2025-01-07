(*    Copied and modified from the original implementation at:

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

let parse = function
  | " " -> Space
  | "\027" -> Escape
  | "\027[A" -> Up
  | "\027[B" -> Down
  | "\027[C" -> Right
  | "\027[D" -> Left
  | "\127" -> Backspace
  | "\n" -> Enter
  | key -> Key key

let read_key key =
  match key with
  | "\027" -> (
      match Tty.Stdin.read_utf8 () with
      | `Read "[" -> (
          match Tty.Stdin.read_utf8 () with
          | `Read key -> parse ("\027[" ^ key)
          | _ -> parse key)
      | _ -> parse key)
  | "\n" -> parse key
  | key -> parse key

let read () =
  match Tty.Stdin.read_utf8 () with
  | `Read key -> `Read (read_key key)
  | (`Retry | `End | `Malformed _) as other -> other
