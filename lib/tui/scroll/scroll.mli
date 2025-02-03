type t

type sections = {
  before : int;
  scroll : int;
  after : int;
}

(** [make ~height ~span ~lines ~offset] creates a new scrollbar from the given
    parameters.

    - [height]: the height of scrollbar in characters
    - [span]: number of lines to display on the screen
    - [lines]: total number of lines in the file
    - [offset]: number of skipped lines before the first to show

    {b NOTE}: Returns [None] if [height >= lines] *)
val make : height:int -> span:int -> lines:int -> offset:int -> t option

val to_sections : t -> sections

(** Renders a scrollbar from [sections]. *)
val render : sections -> Pretty.Doc.t
