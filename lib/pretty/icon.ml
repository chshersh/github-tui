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

let hack_nerd_font = "Hack Nerd Font Mono"
let fc_list_cmd = Printf.sprintf "fc-list | grep '%s'" hack_nerd_font
let uname_cmd = {| uname |}

type os_name =
  | Linux
  | MacOS

let uname_result =
  Shell.proc_stdout uname_cmd |> String.trim |> function
  | "Linux" -> Some Linux
  | "Darwin" -> Some MacOS
  | _ -> None

(* Based on the list of possible folders here: https://github.com/adrg/xdg/blob/master/README.md#other-directories*)
let mac_font_dirs =
  [
    "~/Library/Fonts";
    "/Library/Fonts";
    "/System/Library/Fonts";
    "/Network/Library/Fonts";
  ]

let string_contains str substr =
  let re = Str.regexp_string substr in
  try
    ignore (Str.search_forward re str 0);
    true
  with Not_found -> false

let rec font_exists_in_dirs font_name = function
  | [] -> false
  | dir :: dirs ->
      if Sys.file_exists dir then
        let files = Sys.readdir dir in
        if Array.exists (fun file -> string_contains file font_name) files then
          true
        else font_exists_in_dirs font_name dirs
      else font_exists_in_dirs font_name dirs

(* We only support mac and linux right now - if the system is unix based and
it's linux, we can use the fc-list cmd, otherwise on mac we look in common
directories manually for our font *)
let nerd_font_installed =
  match uname_result with
  | Some Linux ->
      Shell.proc_stdout fc_list_cmd |> String.trim |> String.length > 0
  | Some MacOS -> font_exists_in_dirs hack_nerd_font mac_font_dirs
  | None -> false (* do we want this be false or true if we can't tell? *)

let icons =
  if nerd_font_installed then
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
