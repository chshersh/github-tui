(** File contents as an array of lines, where each line is wrapped into a
    document (for rendering efficiency) *)
type t = {
  lines : Pretty.Doc.t array;
  offset : int;
}

type file_type =
  | Binary
  | Text

(** Reads file contents using 'bat' to have pretty syntax highlighting **)
val read : string -> t

(** Returns the len of file contents **)
val length : t -> int

(** Returns file_type based on the first 1024 bytes of the file **)
val type_of_path : string -> file_type
