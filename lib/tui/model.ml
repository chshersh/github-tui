type code_tab = {
  (* Full path to the repository directory *)
  root_dir_path : string;
  (* Zipper of the repository code *)
  fs : Fs.zipper;
}

type issues_tab = { issues : Gh.Issue.t list Lazy.t }

type tab =
  | Code
  | Issues
  | PullRequests

type t = {
  width : int;
  height : int;
  (* GitHub login of the owner: chshersh *)
  owner : string;
  (* GitHub repository name: github-tui *)
  repo : string;
  current_tab : tab;
  code_tab : code_tab;
  issues_tab : issues_tab;
}

type initial_data = {
  owner : string;
  repo : string;
  root_dir_path : string;
  files : Fs.tree array;
  width : int;
  height : int;
}

let initial_model { owner; repo; root_dir_path; files; width; height } =
  {
    width;
    height;
    owner;
    repo;
    current_tab = Code;
    code_tab = { root_dir_path; fs = Fs.zip_it files };
    issues_tab = { issues = lazy (Gh.Issue.issues ~owner ~repo) };
  }
