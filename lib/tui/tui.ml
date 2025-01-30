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
    | Fs.File (path, _, _) ->
        Printf.printf "Given path '%s' is not a directory!" path;
        exit 1
    | Fs.Dir (_, files) -> files
  in
  files

type terminal = {
  height : int;
  width : int;
}

let min_height, min_width = (40, 140)
let size_warning = ref true

let get_terminal_dimensions () =
  match (Terminal_size.get_rows (), Terminal_size.get_columns ()) with
  | Some height, Some width ->
      if !size_warning && (height < min_height || width < min_width) then (
        Printf.printf
          "⚠️ Terminal size is too small! Expected minimum size: %d x %d, but \
           got: %d x %d.\n"
          min_width min_height width height;
        exit 1);
      { height; width }
  | _ ->
      Printf.printf "⚠️ Not able to get the terminal size.\n";
      exit 1

let init ~owner_repo ~local_path : Model.initial_data =
  let ({ owner; repo } as owner_repo) = parse_owner_repo owner_repo in
  let root_dir_path = clone_repo ~owner_repo ~local_path in
  let files = Lazy.force (read_root_tree ~root_dir_path) in
  let { height; width } = get_terminal_dimensions () in
  { owner; repo; root_dir_path; files; width; height }

let start ~owner_repo ~local_path ~log_file ~ignore_size_warning =
  size_warning := not ignore_size_warning;
  let initial_data = init ~owner_repo ~local_path in
  let init = Model.initial_model initial_data in
  let app = Tea.make ~init ~update:Update.update ~view:View.view in
  Tea.run ?path:log_file app
