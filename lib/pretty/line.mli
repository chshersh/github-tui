(** A type defining a single line of text with different parts ("chunks") having
    potentially different formatting. *)
type t

(** Returns the length of a line as in the number of graphemes (before
    formatting applied). *)
val length : t -> int

(** Smart constructor for a line from a list of chunks to calculate the final
    [length] automatically as well. *)
val of_chunks : Chunk.t list -> t

(** Add a chunk to the beginning of a line. *)
val prepend_chunk : Chunk.t -> t -> t

(** Append two lines into a single line. *)
val append : t -> t -> t

(** Format a single line as string, applying formatting. *)
val fmt : t -> string

(** [zip_lines l r] zips two lists of lines horizontally. Meaning:

    - The size of the result is the maximum of both sizes
    - Element concatenated pairwise
    - If the first list is longer, elements from it are taken as is
    - If the second list is longer, the maximum size of elements from the first
      list is added as padding. *)
val zip_lines : t list -> t list -> t list
