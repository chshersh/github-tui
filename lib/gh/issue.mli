type t = {
  number : int;
  title : string;
  author : string;
}

(** Query all open issues *)
val issues : owner:string -> repo:string -> t list
