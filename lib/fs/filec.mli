(** File contents as an array of lines, where each line is wrapped into a
    document (for rendering efficiency) *)
type t =
  | Binary
  | Text of {
      lines : Pretty.doc array;
      offset : int;
    }

(** Reads file contents using 'bat' to have pretty syntax highlighting **)
val read_file_contents : string -> t

(** Returns offset based on file type of contents **)
val offset_from_file_contents : t -> int

(** Returns the len of file contents **)
val line_len_from_file_contents : t -> int

(** Returns the lines of file contents **)
val lines_from_file_contents : t -> Pretty.doc array
