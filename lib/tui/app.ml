let init ~repo ~path : Model.initial_data =
  let tree = Fs.read_tree path in
  let dirname, files =
    match tree with
    | Fs.File path ->
        Printf.printf "Given path '%s' is not a directory!" path;
        exit 1
    | Fs.Dir (dirname, files) -> (dirname, files)
  in
  let terminal_rows = Option.value (Terminal_size.get_rows ()) ~default:120 in
  let terminal_cols =
    Option.value (Terminal_size.get_columns ()) ~default:140
  in
  { repo; dirname; files; terminal_rows; terminal_cols }

let app = Minttea.app ~init:Init.init ~update:Update.update ~view:View.view ()

let start repo path =
  let initial_data = init ~repo ~path in
  let initial_model = Model.initial_model initial_data in
  Minttea.start app ~initial_model
