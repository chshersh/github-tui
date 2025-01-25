(** This module provides utilities to work with colors. *)

(** An RGB colour code *)
type t

(** Returns RGB colour from a hex string like "7057f" or "#fbca04".

    Returns black colour by default if there's a parsing error. *)
val of_hex : string -> t

(** This function calculates the luminance of colour and suggests a foreground
    colour. *)
val foreground : t -> [ `Light | `Dark ]

(** Convert code to escape sequence. *)
val to_escape_seq : t -> string
