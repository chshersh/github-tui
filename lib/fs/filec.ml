type t =
  | Binary
  | Text of {
      lines : Pretty.doc array;
      offset : int;
    }

(* Regex to used to determine if bat outputs a binary file warning. This is a
   bit of a fragile approach, but there is no robust way to determine if a file
   is binary or not. Improve this if it becomes a problem.
*)
let binary_file_pattern = Str.regexp ".*\\[bat warning\\].*Binary.*content*."
let binary_file_warning = "This file is binary and cannot be displayed"

let has_binary_warning contents =
  Str.string_match binary_file_pattern contents 0

let bat_cmd =
  {|bat --style=numbers,changes --color=always --italic-text=always --paging=never --terminal-width=80 |}

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

let offset_from_file_contents = function
  | Text { offset; _ } -> offset
  | Binary -> 0

let line_len_from_file_contents = function
  | Text { lines; _ } -> Array.length lines
  | Binary -> 1

let lines_from_file_contents = function
  | Text { lines; _ } -> lines
  | Binary -> [| Pretty.str binary_file_warning |]
