type t = {
  number : int;
  title : string;
  author : string;
  state : state;
  labels : label list;
}

and label = {
  name : string;
  color : string;
}

and state =
  | Closed
  | Open

(** Query all open issues *)
val issues : owner:string -> repo:string -> t list
