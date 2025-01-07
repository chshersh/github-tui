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

let show_key = function
  | Up -> "<up>"
  | Down -> "<down>"
  | Left -> "<left>"
  | Right -> "<right>"
  | Space -> "<space>"
  | Escape -> "<esc>"
  | Backspace -> "<backspace>"
  | Enter -> "<enter>"
  | Key key -> key

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

type read =
  [ `End
  | `Malformed of string
  | `Read of t
  ]

let show_read = function
  | `End -> "End"
  | `Malformed s -> Printf.sprintf "Malformed: %s" s
  | `Read key -> Printf.sprintf "Key: %s" (show_key key)

let read_key key =
  match key with
  | "\027" -> (
      match Stdin.read_utf8 () with
      | `Read "[" -> (
          match Stdin.read_utf8 () with
          | `Read key -> parse ("\027[" ^ key)
          | _ -> parse key)
      | _ -> parse key)
  | "\n" -> parse key
  | key -> parse key

let read () =
  match Stdin.read_utf8 () with
  | `Read key -> `Read (read_key key)
  | (`End | `Malformed _) as other -> other
