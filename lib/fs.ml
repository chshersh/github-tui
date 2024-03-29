type tree =
  | File of string * string lazy_t
  | Dir of string * tree array

let file_name = function
  | File (name, _) -> name
  | Dir (name, _) -> name

(* A files comparison:

   1. Directories before files
   2. Otherwise, lexicographically
*)
let order_files t1 t2 =
  match (t1, t2) with
  | Dir _, File _ -> -1
  | File _, Dir _ -> 1
  | _, _ -> String.compare (file_name t1) (file_name t2)

let rec sort_tree = function
  | File (name, contents) -> File (name, contents)
  | Dir (name, children) ->
      Array.sort order_files children;
      Dir (name, Array.map sort_tree children)

(* Reads file contents using 'bat' to have pretty syntax highlighting *)
let read_file_contents path =
  let cmd =
    "bat --plain --color=always --italic-text=always --paging=never \
     --terminal-width=80 " ^ path
  in
  Shell.proc_stdout cmd

let rec to_tree path =
  if Sys.is_directory path then
    let children =
      Array.map
        (fun child_name -> to_tree (Filename.concat path child_name))
        (Sys.readdir path)
    in
    let dirname = Filename.basename path in
    Dir (dirname, children)
  else File (Filename.basename path, lazy (read_file_contents path))

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
