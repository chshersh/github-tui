type t =
  | Binary
  | Text of {
      lines : Pretty.Doc.t array;
      offset : int;
    }

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
  if is_likely_binary path then Binary
  else
    let lines =
      path
      |> read_file_raw
      |> String.split_on_char '\n'
      |> List.map Pretty.Doc.str
      |> Array.of_list
    in
    Text { lines; offset = 0 }

type file_type =
  | BinaryFile
  | TextFile

(* Returns file type based on contents *)
let type_of_path path = if is_likely_binary path then BinaryFile else TextFile

(* Returns offset based on file type of contents *)

let offset = function
  | Text { offset; _ } -> offset
  | Binary -> 0

(* Returns the lines of the file contents *)
let lines = function
  | Text { lines; _ } -> lines
  | Binary -> binary_file_warning

(* Returns the number of lines in the file contents *)
let length filec = filec |> lines |> Array.length
