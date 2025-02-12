type t = {
  styles : Style.t;
  string : string;
}

let fmt { styles; string } = Ansifmt.Styling.wrap ~contents:string styles

let replicate width s =
  let styles = Style.none in
  if width <= 0 then { styles; string = "" }
  else
    let filling = Extra.String.repeat_txt width s in
    { styles; string = filling }
