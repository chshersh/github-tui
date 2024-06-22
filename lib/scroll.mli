type t

type sections = {
  before : int;
  scroll : int;
  after : int;
}

val make : height:int -> span:int -> lines:int -> offset:int -> t option
val to_sections : t -> sections
