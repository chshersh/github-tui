let app = Minttea.app ~init:Init.init ~update:Update.update ~view:View.view ()

let start repo path =
  let tree = Fs.read_tree path in
  match tree with
  | Fs.File path ->
      Printf.printf "Given path '%s' is not a directory!" path;
      exit 1
  | Fs.Dir (dirname, files) ->
      let initial_model = Model.initial_model ~repo ~dirname ~files in
      Minttea.start app ~initial_model
