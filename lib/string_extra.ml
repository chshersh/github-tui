let unlines : string list -> string = String.concat "\n"

(* Calculate the line of length using grapheme clusters and ignore ANSI formatting. *)
let width = Shape_the_term.width
let repeat_txt n txt = String.concat "" (List.init n (fun _ -> txt))

let fill_right (n : int) (s : string) : string =
  s ^ repeat_txt (n - width s) " "
