let init ~repo ~path : Model.initial_data =
  let tree = Fs.read_tree path in
  match tree with
  | Fs.File path ->
      Printf.printf "Given path '%s' is not a directory!" path;
      exit 1
  | Fs.Dir (dirname, files) -> { repo; dirname; files }

let app = Minttea.app ~init:Init.init ~update:Update.update ~view:View.view ()

let start repo path =
  let initial_data = init ~repo ~path in
  let initial_model = Model.initial_model initial_data in
  Minttea.start app ~initial_model
