(** Run a given string as an external process with arguments.

    Redirect the called process stdout and stderr to the current process stdout.

    Also print the command with a pretty prompt. *)
val proc : string -> (unit, string) result

(** Run the given CLI command as external process and collect its stdout to the
    resulting string. *)
val proc_stdout : string -> string
