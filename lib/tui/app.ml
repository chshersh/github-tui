let init ~repo ~root_dir_path : Model.initial_data =
  let tree = Fs.read_tree root_dir_path in
  let files =
    match tree with
    | Fs.File (path, _) ->
        Printf.printf "Given path '%s' is not a directory!" path;
        exit 1
    | Fs.Dir (_, files) -> files
  in
  let terminal_rows = Option.value (Terminal_size.get_rows ()) ~default:120 in
  let terminal_cols =
    Option.value (Terminal_size.get_columns ()) ~default:140
  in
  { repo; root_dir_path; files; terminal_rows; terminal_cols }

let app = Minttea.app ~init:Init.init ~update:Update.update ~view:View.view ()

let start repo root_dir_path =
  let initial_data = init ~repo ~root_dir_path in
  let initial_model = Model.initial_model initial_data in
  Minttea.start app ~initial_model
