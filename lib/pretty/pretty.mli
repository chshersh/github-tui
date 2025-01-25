(** A library for declarative UI definition. *)

(** A module containing declarative UI definition. The elements provide
    flexibility. *)
module Doc = Doc

(** Similar to {!Doc} but allows less flexibility. However, it has the advantage
    that the sizes (both width and height) of all subelements is known.

    {!Layout.t} is the output of {!Doc.render} with all the sizes calculated. *)
module Layout = Layout

(** Color-related functions. *)
module Color = Color

(** Render a document into the final string

    All notes from {!Doc.render} apply. *)
val render : width:int -> Doc.t -> string
