type styles = ANSITerminal.style list

type t = {
  styles : styles;
  string : string;
}

let fmt { styles; string } = ANSITerminal.sprintf styles "%s" string

let replicate width s =
  if width <= 0 then { styles = []; string = "" }
  else
    let filling = Extra.String.repeat_txt width s in
    { styles = []; string = filling }
