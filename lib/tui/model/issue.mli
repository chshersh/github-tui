(** Model types and functions for the issue tab. *)

(** Main issue tab. Fields:

    - [all_issues]: Lazily fetched all issues
    - [filter]: Filter for currently selected issues
    - [issues]: Filtered issues from [all_issues] using [filter]
    - [offset]: Offset for currently selected issue from [issues] *)
type t = {
  all_issues : Gh.Issue.t list Lazy.t;
  filter : filter;
  issues : Gh.Issue.t list Lazy.t;
  offset : int;
  error : Gh.Client.error option;
}

and filter =
  | State of Gh.Issue.state
  | All

val filter_all : filter
val filter_open : filter
val filter_closed : filter
val make : owner:string -> repo:string -> t

(** Change the filter to a new one and update currently selected issues. *)
val apply_filter : filter -> t -> t
