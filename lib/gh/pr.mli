type pr_state =
  | Merged
  | Open
  | Closed

type t = {
  number : int;
  title : string;
  author : string;
  state : pr_state option;
}

(** Query all open pull requests *)
val pull_requests : owner:string -> repo:string -> t list
