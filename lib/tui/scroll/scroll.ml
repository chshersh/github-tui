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
  (* Edge case: end of the scroll*)
  if offset + span >= lines then
    let scroll = span * height / lines in
    let before = height - scroll in
    let after = 0 in
    { before; scroll; after }
  else
    let before = offset * height / lines in
    let scroll = span * height / lines in
    let after = height - before - scroll in
    { before; scroll; after }
