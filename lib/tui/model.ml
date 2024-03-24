type code_tab = {
  (* Repository directory *)
  dirname : string;
  (* Zipper of the repository code *)
  fs : Fs.zipper;
}

type tab =
  | Code
  | Issues
  | PullRequests

type t = {
  terminal_rows : int;
  terminal_cols : int;
  repo : string;
  current_tab : tab;
  code_tab : code_tab;
}

type initial_data = {
  repo : string;
  dirname : string;
  files : Fs.tree array;
  terminal_rows : int;
  terminal_cols : int;
}

let initial_model { repo; dirname; files; terminal_rows; terminal_cols } =
  {
    terminal_rows;
    terminal_cols;
    repo;
    current_tab = Code;
    code_tab = { dirname; fs = Fs.zip_it files };
  }
