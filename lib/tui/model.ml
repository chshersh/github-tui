type code_tab = {
  (* Full path to the repository directory *)
  root_dir_path : string;
  (* Zipper of the repository code *)
  fs : Fs.zipper;
}

type tab =
  | Code
  | Issues
  | PullRequests

type t = {
  exit : bool;
  width : int;
  height : int;
  repo : string;
  current_tab : tab;
  code_tab : code_tab;
}

type initial_data = {
  repo : string;
  root_dir_path : string;
  files : Fs.tree array;
  width : int;
  height : int;
}

let initial_model { repo; root_dir_path; files; width; height } =
  {
    width;
    height;
    repo;
    exit = false;
    current_tab = Code;
    code_tab = { root_dir_path; fs = Fs.zip_it files };
  }
