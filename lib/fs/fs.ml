module Filec = Filec

type tree =
  | File of {
      name : string;
      contents : Filec.t Lazy.t;
      file_type : Filec.file_type Lazy.t;
    }
  | Dir of {
      name : string;
      children : tree array Lazy.t;
    }

type dir_cursor = {
  pos : int;
  files : tree array;
}

type cursor =
  | Dir_cursor of dir_cursor
  | File_cursor of Filec.t

(* Extracts the file name from a tree node *)
let file_name = function
  | File { name; _ } -> name
  | Dir { name; _ } -> name

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
  | File _ as f -> f
  | Dir { name; children = (lazy children) } ->
      Array.sort order_files children;
      Dir { name; children = lazy (Array.map sort_tree children) }

(* Recursively reads a directory tree *)
let rec to_tree path =
  if Sys.is_directory path then
    let children =
      lazy
        (Array.map
           (fun child_name -> to_tree (Filename.concat path child_name))
           (Sys.readdir path))
    in
    let dirname = Filename.basename path in
    Dir { name = dirname; children }
  else
    File
      {
        name = Filename.basename path;
        contents = lazy (Filec.read path);
        file_type = lazy (Filec.type_of_path path);
      }

let read_tree path = path |> to_tree |> sort_tree
let file_at cursor = cursor.files.(cursor.pos)

type zipper = {
  parents : dir_cursor list;
  current : cursor;
}

let zip_it trees =
  { parents = []; current = Dir_cursor { pos = 0; files = trees } }

let zipper_parents zipper =
  List.map (fun cursor -> file_name (file_at cursor)) zipper.parents

(* TODO: Horrible hardcoding of maximum lines view *)
let span = 40

let move_cursor move_dir_cursor move_file_cursor = function
  | Dir_cursor cursor -> Dir_cursor (move_dir_cursor cursor)
  | File_cursor cursor -> File_cursor (move_file_cursor cursor)

let move_dir_cursor move cursor =
  let len = Array.length cursor.files in
  let new_pos = (cursor.pos + move + len) mod len in
  { cursor with pos = new_pos }

let move_file_cursor move cursor =
  let len = Filec.length cursor in
  let new_offset = cursor.offset + move in
  if new_offset < 0 || new_offset + span > len then cursor
  else { cursor with offset = new_offset }

let go_move move zipper =
  let move_dir = move_dir_cursor move in
  let move_file = move_file_cursor move in
  let old = zipper.current in
  let new_cursor = move_cursor move_dir move_file old in
  { zipper with current = new_cursor }

let go_down = go_move 1
let go_up = go_move (-1)

let go_next zipper =
  match zipper.current with
  | File_cursor _ -> zipper
  | Dir_cursor cursor -> (
      let next = file_at cursor in
      match next with
      | File { contents; _ } ->
          {
            parents = cursor :: zipper.parents;
            current = File_cursor (Lazy.force contents);
          }
      | Dir { children = (lazy next); _ } ->
          if Array.length next = 0 then zipper
          else
            {
              parents = cursor :: zipper.parents;
              current = Dir_cursor { pos = 0; files = next };
            })

let go_back zipper =
  match zipper.parents with
  | [] -> zipper
  | current :: parents -> { parents; current = Dir_cursor current }
