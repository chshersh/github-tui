module Style = Style

(** A type representing a structured document with known sizes for all
    sub-elements. *)
type t

(** Width of the element. *)
val width : t -> int

(** Height of the element. *)
val height : t -> int

(** Create a single chunk of string without formatting. *)
val str : string -> t

(** Create a single chunk of string with formatting. *)
val fmt : Style.t -> string -> t

(** Put all documents in a list horizontally, automatically adding required
    padding. *)
val horizontal : t list -> t

(** Put all documents in a list vertically after each other with a line
    separator. *)
val vertical : t list -> t

(** Convert the layout to a list of lines. *)
val to_lines : t -> string list
