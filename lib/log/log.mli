(** Log a single line in the following format to the given file if passed.

    {v
timestamp=1447865947.56802297 tag=event msg=Retry
    v} *)
val log : ?path:string -> tag:string -> string -> unit
