(** This module contains generic widgets. *)

(** Created a vertical list of items inside border like this one:

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

    This function takes values of type {!Pretty.Layout.t}, so it could
    efficiently calculate the size of element per line. *)
val vlist_border : Pretty.Layout.t list -> Pretty.Doc.t
