type t = {
  lines : Pretty.Doc.t array;
  offset : int;
}

type file_type =
  | Binary
  | Text

let binary_file_warning =
  [| Pretty.Doc.str "This file is binary and cannot be displayed" |]

let open_file_bin num_bytes path =
  let buffer = Bytes.create num_bytes in
  let ic = open_in_bin path in
  let n = input ic buffer 0 num_bytes in
  close_in ic;
  Bytes.sub buffer 0 n

let has_zero_bytes buffer =
  let rec has_null = function
    | i when i = Bytes.length buffer -> false
    | i when Bytes.get buffer i = '\x00' -> true
    | i -> has_null (i + 1)
  in
  has_null 0

let is_likely_binary path = path |> open_file_bin 1024 |> has_zero_bytes

let bat_cmd =
  {|bat --style=numbers,changes --color=always --italic-text=always --paging=never --terminal-width=80 |}

let read_file_raw path = Shell.proc_stdout (bat_cmd ^ path)

(* Reads file contents using 'bat' to have pretty syntax highlighting *)
let read path =
  if is_likely_binary path then { lines = binary_file_warning; offset = 0 }
  else
    let lines =
      path
      |> read_file_raw
      |> String.split_on_char '\n'
      |> List.map Pretty.Doc.str
      |> Array.of_list
    in
    { lines; offset = 0 }

(* Returns file type based on contents *)
let type_of_path path = if is_likely_binary path then Binary else Text

(* Returns the number of lines in the file contents *)
let length filec = filec.lines |> Array.length
