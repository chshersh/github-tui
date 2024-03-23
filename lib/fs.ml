type tree =
  | File of string
  | Dir of string * tree array

let file_name = function
  | File path -> path
  | Dir (path, _) -> path

(* A files comparison:

   1. Directories before files
   2. Otherwise, lexicographically
*)
let order_files t1 t2 =
  match (t1, t2) with
  | Dir _, File _ -> -1
  | File _, Dir _ -> 1
  | File name_1, File name_2 -> String.compare name_1 name_2
  | Dir (name_1, _), Dir (name_2, _) -> String.compare name_1 name_2

let rec sort_tree = function
  | File name -> File name
  | Dir (name, children) ->
      Array.sort order_files children;
      Dir (name, Array.map sort_tree children)

let rec to_tree path =
  if Sys.is_directory path then
    let children =
      Array.map
        (fun child_name -> to_tree (Filename.concat path child_name))
        (Sys.readdir path)
    in
    let dirname = Filename.basename path in
    Dir (dirname, children)
  else File (Filename.basename path)

let read_tree path = path |> to_tree |> sort_tree

type cursor = {
  pos : int;
  files : tree array;
}

let file_at cursor = cursor.files.(cursor.pos)

type zipper = {
  parents : cursor list;
  current : cursor;
}

let zip_it trees = { parents = []; current = { pos = 0; files = trees } }

let zipper_parents zipper =
  List.map (fun cursor -> file_name (file_at cursor)) zipper.parents

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

let go_next zipper =
  let cursor = zipper.current in
  let next = file_at cursor in
  match next with
  | File _ -> zipper
  | Dir (_, next) ->
      if Array.length next = 0 then zipper
      else
        {
          parents = cursor :: zipper.parents;
          current = { pos = 0; files = next };
        }

let go_back zipper =
  match zipper.parents with
  | [] -> zipper
  | current :: parents -> { parents; current }
