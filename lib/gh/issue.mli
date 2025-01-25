type label = {
  name : string;
  color : string;
}

type t = {
  number : int;
  title : string;
  author : string;
  labels : label list;
}

(** Query all open issues *)
val issues : owner:string -> repo:string -> t list
