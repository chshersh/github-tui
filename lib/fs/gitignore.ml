module Gitignore = Gitignore

type rule = {
  pattern : string;
  negated : bool;
}

let strip_prefix ~prefix text =
  if String.starts_with ~prefix text then
    String.sub text (String.length prefix)
      (String.length text - String.length prefix)
  else text

let normalize_path path =
  path
  |> strip_prefix ~prefix:"./"
  |> strip_prefix ~prefix:Filename.dir_sep

let find_gitignore path =
  let full_path = Filename.concat path ".gitignore" in
  if Sys.file_exists full_path then (
    let ic = open_in full_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content)
  else None

let parse (content : string) : rule list =
  content
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line ->
         line <> "" && not (String.starts_with ~prefix:"#" line))
  |> List.map (fun line ->
         if String.starts_with ~prefix:"!" line then
           {
             pattern = String.sub line 1 (String.length line - 1);
             negated = true;
           }
         else { pattern = line; negated = false })

let matches path rule =
  let normalized_path = normalize_path path in
  let normalized_pattern = normalize_path rule.pattern in
  if normalized_pattern = "" then false
  else if String.ends_with ~suffix:Filename.dir_sep normalized_pattern then
    String.starts_with ~prefix:normalized_pattern normalized_path
  else
    try
      let re = Re.Glob.glob ~anchored:true normalized_pattern |> Re.compile in
      Re.execp re normalized_path
    with _ -> false

let is_ignored path rules =
  let rec apply rules ignored =
    match rules with
    | [] -> ignored
    | rule :: rest ->
        if matches path rule then apply rest (not rule.negated)
        else apply rest ignored
  in
  apply rules false
