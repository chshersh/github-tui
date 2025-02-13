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

let fc_list_cmd = {| fc-list | grep 'Hack Nerd Font Mono' |}

(** Use fc-list to check if Hack Nerd Font is installed - this might be a little
    brittle, but it's simple - update if it becomes an issue. **)
let nerd_font_installed () =
  fc_list_cmd |> Shell.proc_stdout |> String.trim |> String.length > 0

let icons =
  if nerd_font_installed () then
    {
      closed_char = "\u{ebda}";
      open_char = "\u{ea64}";
      merged_char = "\u{e725}";
      arrow_left_char = "\u{f0a8}";
      pwd_char = "\u{e5fd}";
      dir_char = "\u{f4d4}";
      empty_dir_char = "\u{f413}";
      file_char = "\u{f4a5}";
      bin_char = "\u{eae8}";
      warning = "\u{26A0}";
      issue_char = "\u{f41b}";
    }
  else
    {
      closed_char = "x";
      open_char = "o";
      merged_char = "m";
      arrow_left_char = "<-";
      pwd_char = "*";
      dir_char = "/";
      empty_dir_char = "/";
      file_char = "f";
      bin_char = "b";
      warning = "!";
      issue_char = "i";
    }
