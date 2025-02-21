(** A library for declarative UI definition. *)

(** Common styles for GitHub TUI. *)
module Style = Style

(** A module containing declarative UI definition. The elements provide
    flexibility. *)
module Doc = Doc

(** Similar to {!Doc} but allows less flexibility. However, it has the advantage
    that the sizes (both width and height) of all subelements is known.

    {!Layout.t} is the output of {!Doc.render} with all the sizes calculated. *)
module Layout = Layout

(** A record representign the Icons and symbols used for identifying different
    parts. Symbols from Hack Nerd Font Mono are used when it's available.
    Symbols list:

    - https://www.nerdfonts.com/cheat-sheet *)
val icons : Icon.t

(** Render a document into the final string

    All notes from {!Doc.render} apply. *)
val render : width:int -> height:int -> Doc.t -> string
