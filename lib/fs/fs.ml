module Filec = Filec
module Gitignore = Gitignore

type tree =
  | File of {
      name : string;
      contents : Filec.t Lazy.t;
      file_type : Filec.file_type Lazy.t;
      ignored : bool;
    }
  | Dir of {
      name : string;
      children : tree array Lazy.t;
      ignored : bool;
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
  | Dir { name; children = (lazy children); ignored } ->
      Array.sort order_files children;
      Dir { name; children = lazy (Array.map sort_tree children); ignored }

let filter_visible ~show_ignored files =
  if show_ignored then files
  else
    files
    |> Array.to_list
    |> List.filter (function
         | File { ignored = true; _ } | Dir { ignored = true; _ } -> false
         | _ -> true)
    |> Array.of_list

let relative_path ~root path =
  let prefix =
    if Filename.check_suffix root Filename.dir_sep then root
    else root ^ Filename.dir_sep
  in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

(* Recursively reads a directory tree *)
let rec to_tree ~patterns ~root path =
  let name = Filename.basename path in
  let rel_path =
    let rel = relative_path ~root path in
    if Sys.is_directory path then rel ^ Filename.dir_sep else rel
  in
  let ignored = Gitignore.is_ignored rel_path patterns in
  if Sys.is_directory path then
    let children =
      lazy
        (Sys.readdir path
        |> Array.to_list
        |> List.map (Filename.concat path)
        |> List.map (to_tree ~patterns ~root)
        |> Array.of_list)
    in
    Dir { name; ignored; children }
  else
    File
      {
        name;
        ignored;
        contents = lazy (Filec.read path);
        file_type = lazy (Filec.type_of_path path);
      }

let ignore_patterns path =
  match Gitignore.find_gitignore path with
  | Some content -> Gitignore.parse content
  | None -> []

let read_tree path =
  to_tree ~patterns:(ignore_patterns path) ~root:path path |> sort_tree

let file_at cursor = cursor.files.(cursor.pos)

type zipper = {
  parents : dir_cursor list;
  current : cursor;
  show_ignored : bool;
}

let zip_it trees ~show_ignored =
  let visible = filter_visible ~show_ignored trees in
  {
    parents = [];
    current = Dir_cursor { pos = 0; files = visible };
    show_ignored;
  }

let zipper_parents zipper =
  List.map (fun cursor -> file_name (file_at cursor)) zipper.parents

(* TODO: Horrible hardcoding of maximum lines view *)
let span = 40

let move_cursor move_dir_cursor move_file_cursor = function
  | Dir_cursor cursor -> Dir_cursor (move_dir_cursor cursor)
  | File_cursor cursor -> File_cursor (move_file_cursor cursor)

let move_dir_cursor move cursor =
  let len = Array.length cursor.files in
  if len = 0 then cursor
  else
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
  | Dir_cursor cursor ->
      if Array.length cursor.files = 0 then zipper
      else
        let next = file_at cursor in
        match next with
        | File { contents; _ } ->
            {
              parents = cursor :: zipper.parents;
              current = File_cursor (Lazy.force contents);
              show_ignored = zipper.show_ignored;
            }
        | Dir { children = (lazy children); _ } ->
            let visible =
              filter_visible ~show_ignored:zipper.show_ignored children
            in
            if Array.length visible = 0 then zipper
            else
              {
                parents = cursor :: zipper.parents;
                current = Dir_cursor { pos = 0; files = visible };
                show_ignored = zipper.show_ignored;
              }

let go_back zipper =
  match zipper.parents with
  | [] -> zipper
  | current :: parents ->
      {
        parents;
        current = Dir_cursor current;
        show_ignored = zipper.show_ignored;
      }
