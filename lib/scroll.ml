type t = {
  height : int;
  span : int;
  lines : int;
  offset : int;
}

type sections = {
  before : int;
  scroll : int;
  after : int;
}

let make ~height ~span ~lines ~offset =
  if lines <= height then None else Some { height; span; lines; offset }

let to_sections { height; span; lines; offset } =
  let before = offset * height / lines in
  let scroll = span * height / lines in
  let after = (lines - span - offset) * height / lines in
  { before; scroll; after }
