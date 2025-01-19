type owner_repo = {
  owner : string;
  repo : string;
}

let parse_owner_repo owner_repo =
  match String.split_on_char '/' owner_repo with
  | [ owner; repo ] -> { owner; repo }
  | _ ->
      Printf.printf "❌ Expected <owner>/<repo> but got: '%s'\n" owner_repo;
      exit 1

let clone_repo ~owner_repo ~local_path =
  match local_path with
  | Some path -> path
  | None ->
      let { owner; repo } = owner_repo in
      let temp_dir =
        Filename.temp_dir "github-tui-" (Printf.sprintf "%s-%s" owner repo)
      in
      let cmd =
        Printf.sprintf "git clone git@github.com:%s/%s.git %s" owner repo
          temp_dir
      in
      Shell.proc cmd;
      temp_dir

let read_root_tree ~root_dir_path =
  let tree = Fs.read_tree root_dir_path in
  let files =
    match tree with
    | Fs.File (path, _) ->
        Printf.printf "Given path '%s' is not a directory!" path;
        exit 1
    | Fs.Dir (_, files) -> files
  in
  files

type terminal = {
  height : int;
  width : int;
}

let get_terminal_dimensions () =
  let height = Option.value (Terminal_size.get_rows ()) ~default:40 in
  let width = Option.value (Terminal_size.get_columns ()) ~default:140 in
  { height; width }

let init ~owner_repo ~local_path : Model.initial_data =
  let ({ owner; repo } as owner_repo) = parse_owner_repo owner_repo in
  let root_dir_path = clone_repo ~owner_repo ~local_path in
  let files = read_root_tree ~root_dir_path in
  let { height; width } = get_terminal_dimensions () in
  { owner; repo; root_dir_path; files; width; height }

let start ~owner_repo ~local_path ~log_file =
  let initial_data = init ~owner_repo ~local_path in
  let init = Model.initial_model initial_data in
  let app = Tea.make ~init ~update:Update.update ~view:View.view in
  Tea.run ?path:log_file app
