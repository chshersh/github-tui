(** Formatting of a string. *)
type styles = ANSITerminal.style list

(** A part of a line with the formatting added to it. *)
type chunk = {
  styles : styles;
  string : string;
}

(** [padding_chunk n] created a [chunk] without formatting of [n] spaces. *)
val padding_chunk : int -> chunk

(** A type defining a single line of text with different parts ("chunks") having
potentially different formatting. *)
type t

(** Returns the length of a string as in the number of graphemes (before
formatting applied). *)
val length : t -> int

(** Smart constructor for a line from a list of chunks to calculate the final
[length] automatically as well. *)
val of_chunks : chunk list -> t

(** Add a chunk to the beginning of a line. *)
val prepend_chunk : chunk -> t -> t

(** Append two lines into a single line. *)
val append : t -> t -> t

(** Format a single line as string, applying formatting. *)
val fmt : t -> string
