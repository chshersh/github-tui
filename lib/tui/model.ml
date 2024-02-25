type code_tab =
  {
    (* Repository directory *)
    dirname: string;
    (* Zipper of the repository code *)
    fs: Fs.zipper ;
  }

type tab =
  | Code
  | Issues
  | PullRequests

type t =
  { repo: string ;
    current_tab: tab ;
    code_tab: code_tab ;
  }

let initial_model ~repo ~dirname ~files =
  {
     repo ;
     current_tab = Code;
     code_tab = {
      dirname;
      fs = Fs.zip_it files;
     };
  }
