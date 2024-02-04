type tree =
  | File of string
  | Dir of string * tree array

let file_name = function
  | File path -> path
  | Dir (path, _) -> path

type cursor = 
  {
     pos: int;
     files: tree array;
  }

type zipper =
   {
     parents: cursor list;
     current: cursor;
   }

let go_down zipper = 
  let cursor = zipper.current in
  let len = Array.length cursor.files in
  let new_pos = (cursor.pos + 1) mod len in
  let new_cursor = { cursor with pos = new_pos } in
  { zipper with current = new_cursor }

let go_up zipper = 
  let cursor = zipper.current in
  let len = Array.length cursor.files in
  let new_pos = (cursor.pos + len - 1) mod len in
  let new_cursor = { cursor with pos = new_pos } in
  { zipper with current = new_cursor }
