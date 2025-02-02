(** This module contains generic widgets. *)

(** Creates a vertical list of items inside border like this one:

    {v
╭────────╮
│ bin    │
├────────┤
│ images │
├────────┤
│ lib    │
├────────┤
│ src    │
╰────────╯
    v}

    Additionally, highlights the border of the selected item.

    Also adds a scrollbar if items don't fit on the screen.

    This function takes values of type {!Pretty.Layout.t}, so it could
    efficiently calculate the size of element per line. *)
val vlist_border :
  scroll_start:int -> selected:int -> Pretty.Layout.t array -> Pretty.Doc.t
