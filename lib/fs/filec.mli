(** File contents as an array of lines, where each line is wrapped into a
    document (for rendering efficiency) *)
type t =
  | Binary
  | Text of {
      lines : Pretty.doc array;
      offset : int;
    }

(** Reads file contents using 'bat' to have pretty syntax highlighting **)
val read : string -> t

(** Returns offset based on file type of contents **)
val offset : t -> int

(** Returns the len of file contents **)
val length : t -> int

(** Returns the lines of file contents **)
val lines : t -> Pretty.doc array
