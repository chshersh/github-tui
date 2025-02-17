let start args =
  let initial_data = Init.init args in
  let init = Model.initial_model initial_data in
  let app = Tea.make ~init ~update:Update.update ~view:View.view in
  Tea.run ?path:args.log_file app
