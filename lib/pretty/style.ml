module Color = Ansifmt.Color
module Styling = Ansifmt.Styling

type t = Styling.t

let ( & ) = Styling.( & )

(* Empty style *)
let none = Styling.none

(* Repository name. *)
let repo = Styling.(bold & fg Color.blue)

(* A selected element: text or a bordered widget. *)
let selected = Styling.(bold & fg Color.green)

(* Cloned repository directory. *)
let directory = Styling.(bold & fg Color.magenta)

(* When a file in a tree viewer is chosen. *)
let chosen = Styling.(bold & fg Color.magenta)

(* Additional helper text. *)
let secondary = Styling.(fg Color.cyan)
let bold = Styling.bold
let issue_open = Styling.(fg Color.green)
let issue_closed = Styling.(fg Color.red)
let pr_closed = Styling.(bold & fg Color.red)
let pr_open = Styling.(bold & fg Color.green)
let pr_merged = Styling.(bold & fg Color.magenta)

let apply_background_hex_color ~hex text =
  let color = Color.make_rgb_hex hex in
  match color with
  | None -> (text, `Light)
  | Some color ->
      let foreground_suggestion = Color.best_for_contrast ~threshold:80 color in
      let contents = Printf.sprintf " %s " text in
      let text = Styling.(wrap ~contents (bg color)) in
      (text, foreground_suggestion)

let color_of_theme = function
  | `Light -> Styling.(fg Color.white)
  | `Dark -> Styling.(fg Color.black)
