let init ~repo ~root_dir_path : Model.initial_data =
  let tree = Fs.read_tree root_dir_path in
  let files =
    match tree with
    | Fs.File (path, _) ->
        Printf.printf "Given path '%s' is not a directory!" path;
        exit 1
    | Fs.Dir (_, files) -> files
  in
  let height = Option.value (Terminal_size.get_rows ()) ~default:120 in
  let width = Option.value (Terminal_size.get_columns ()) ~default:140 in
  { repo; root_dir_path; files; width; height }

let app = Minttea.app ~init:Init.init ~update:Update.update ~view:View.view ()

let start repo root_dir_path =
  let initial_data = init ~repo ~root_dir_path in
  let initial_model = Model.initial_model initial_data in
  let config = Minttea.make_config ~fps:1 () in
  Minttea.start ~config app ~initial_model
