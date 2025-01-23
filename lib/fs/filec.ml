type t =
  | Binary
  | Text of {
      lines : Pretty.Doc.t array;
      offset : int;
    }

(* Regex to used to determine if bat outputs a binary file warning. This is a
   bit of a fragile approach, but there is no robust way to determine if a file
   is binary or not. Improve this if it becomes a problem.
*)
let binary_file_pattern = Str.regexp ".*\\[bat warning\\].*Binary.*content*."

let binary_file_warning =
  [| Pretty.Doc.str "This file is binary and cannot be displayed" |]

let has_binary_warning contents =
  Str.string_match binary_file_pattern contents 0

let bat_cmd =
  {|bat --style=numbers,changes --color=always --italic-text=always --paging=never --terminal-width=80 |}

let read_file_raw path = Shell.proc_stdout (bat_cmd ^ path)

(* Reads file contents using 'bat' to have pretty syntax highlighting *)
let read path =
  let contents = read_file_raw path in
  if has_binary_warning contents then Binary
  else
    let lines =
      contents
      |> String.split_on_char '\n'
      |> List.map Pretty.Doc.str
      |> Array.of_list
    in
    Text { lines; offset = 0 }

let offset = function
  | Text { offset; _ } -> offset
  | Binary -> 0

(* Returns the lines of the file contents *)
let lines = function
  | Text { lines; _ } -> lines
  | Binary -> binary_file_warning

(* Returns the number of lines in the file contents *)
let length filec = filec |> lines |> Array.length
