(** A part of a line with the formatting added to it. *)
type t = {
  styles : Style.t;
  string : string;
}

(** Convert a chunk to string, applying formatting *)
val fmt : t -> string

(** [replicate_chunk n s] creates a chunk without formatting of [n] repeated
    strings [s]. *)
val replicate : int -> string -> t
