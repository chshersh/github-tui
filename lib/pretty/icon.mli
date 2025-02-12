(** A record representing the icon set **)
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

(** Populated with the current icon set based on if Nerd Font is available or or
    not**)
val icons : t
