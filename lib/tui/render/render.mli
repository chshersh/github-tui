(** This module describes functions to render domain types.

    It surves several purposes:

    + Separate rendering from domain and fetching logic
    + Prerender elements that don't change on the TUI start *)

(** Stores the item and the rendering of this item. *)
type 'a t = {
  item : 'a;
  layout : Pretty.Layout.t;
}

(** [issue_items issues] renders issues*)
val issues : Gh.Issue.t list -> Pretty.Icon.t -> Gh.Issue.t t list
