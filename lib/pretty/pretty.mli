module Style = Style

(** A type representing a structured document. *)
type doc

(** Create a single chunk of string without formatting. *)
val str : string -> doc

(** Create a single chunk of string with formatting. *)
val fmt : Style.t -> string -> doc

(** Put all documents in a list horizontally, automatically adding required
    padding. *)
val horizontal : doc list -> doc

(** Put all documents in a list vertically after each other with a line
    separator. *)
val vertical : doc list -> doc

(** [horizontal_fill filler] fills all the empty horizontal space with the given
    string [filler]. Useful for alignment or separators.

    {b NOTE:} Only one [horizontal_fill] is allowed per [horizontal] element.
    The first one will be used, the remaining ones will be ignored. *)
val horizontal_fill : string -> doc

(** Render the resulting document.

    Parameters:

    * [width]: the max allowed width for the document. Passed recursively and
    currently only used for [horizontal_fill]. *)
val render : width:int -> doc -> string
