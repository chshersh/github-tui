let unlines : string list -> string = String.concat "\n"

let graphemes_len = Shape_the_term.width

let repeat_txt n txt = String.concat "" (List.init n (fun _ -> txt))

let fill_right (n : int) (s : string) : string =
  s ^ repeat_txt (n - graphemes_len s) " "
