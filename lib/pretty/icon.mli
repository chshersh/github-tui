(** A record representing the Icons and symbols used for identifying different
    parts. Symbols from Hack Nerd Font Mono are used when it's not explicitly
    disabled.

    Symbols list:

    - https://www.nerdfonts.com/cheat-sheet *)
type t = {
  closed_char : string;
  open_char : string;
  merged_char : string;
  arrow_left_char : string;
  pwd_char : string;
  dir_char : string;
  empty_dir_char : string;
  file_char : string;
  bin_char : string;
  warning : string;
  issue_char : string;
}

(* Get the icon set - either nerd font or text icons *)
val get_icons : bool -> t
