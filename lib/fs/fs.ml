type file_contents =
  | Binary
  | Text of {
      lines : Pretty.doc array;
      offset : int;
    }

type tree =
  | File of string * file_contents Lazy.t
  | Dir of string * tree array Lazy.t

(* Regex to used to determine if bat outputs a binary file warning. This is a
   bit of a fragile approach, but there is no robust way to determine if a file
   is binary or not. Improve this if it becomes a problem.
*)
let binary_file_pattern = Str.regexp ".*\\[bat warning\\].*Binary.*content*."
let binary_file_warning = "This file is binary and cannot be displayed"

let bat_cmd =
  {|bat --style=numbers,changes --color=always --italic-text=always --paging=never --terminal-width=80 |}

let has_binary_warning contents =
  Str.string_match binary_file_pattern contents 0

(* Extracts the file name from a tree node *)

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
  | Dir (name, (lazy children)) ->
      Array.sort order_files children;
      Dir (name, lazy (Array.map sort_tree children))

let read_file_raw path = Shell.proc_stdout (bat_cmd ^ path)

(* Reads file contents using 'bat' to have pretty syntax highlighting *)
let read_file_contents path =
  let contents = read_file_raw path in
  if has_binary_warning contents then Binary
  else
    let lines =
      contents
      |> String.split_on_char '\n'
      |> List.map Pretty.str
      |> Array.of_list
    in
    Text { lines; offset = 0 }

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
    Dir (dirname, children)
  else File (Filename.basename path, lazy (read_file_contents path))

let read_tree path = path |> to_tree |> sort_tree

type dir_cursor = {
  pos : int;
  files : tree array;
}

type cursor =
  | Dir_cursor of dir_cursor
  | File_cursor of file_contents

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

let update_cursor cursor offset =
  match cursor with
  | Text cur -> File_cursor (Text { cur with offset })
  | Binary -> File_cursor Binary

let offset_from_file_contents = function
  | Text { offset; _ } -> offset
  | Binary -> 0

let line_len_from_file_contents = function
  | Text { lines; _ } -> Array.length lines
  | Binary -> 1

let lines_from_file_contents = function
  | Text { lines; _ } -> lines
  | Binary -> [| Pretty.str binary_file_warning |]

let go_down zipper =
  match zipper.current with
  | Dir_cursor cursor ->
      let len = Array.length cursor.files in
      let new_pos = (cursor.pos + 1) mod len in
      let new_cursor = Dir_cursor { cursor with pos = new_pos } in
      { zipper with current = new_cursor }
  | File_cursor cursor ->
      let len = line_len_from_file_contents cursor in
      let new_offset = offset_from_file_contents cursor + 1 in
      if new_offset + span > len then zipper
      else { zipper with current = update_cursor cursor new_offset }

let go_up zipper =
  match zipper.current with
  | Dir_cursor cursor ->
      let len = Array.length cursor.files in
      let new_pos = (cursor.pos + len - 1) mod len in
      let new_cursor = Dir_cursor { cursor with pos = new_pos } in
      { zipper with current = new_cursor }
  | File_cursor cursor ->
      let new_offset = max 0 (offset_from_file_contents cursor - 1) in
      { zipper with current = update_cursor cursor new_offset }

let go_next zipper =
  match zipper.current with
  | File_cursor _ -> zipper
  | Dir_cursor cursor -> (
      let next = file_at cursor in
      match next with
      | File (_name, contents) ->
          {
            parents = cursor :: zipper.parents;
            current = File_cursor (Lazy.force contents);
          }
      | Dir (_, (lazy next)) ->
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
