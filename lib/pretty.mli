(** A type representing a structured document. *)
type doc

(** Formatting of a string. *)
type styles = ANSITerminal.style list

(** Create a single chunk of string without formatting. *)
val str : string -> doc

(** Create a single chunk of string with formatting. *)
val fmt : styles -> string -> doc

(** Put all documents in a list horizontally, automatically adding required padding. *)
val horizontal : doc list -> doc

(** Put all documents in a list vertically after each other with a line separator. *)
val vertical : doc list -> doc

(** Render the resulting document. *)
val render : doc -> string
