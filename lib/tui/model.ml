type code_tab =
  {
    pos: int ;
    files: string list ;
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

let initial_model repo: t =
  {
     repo ;
     current_tab = Code;
     code_tab = {
      pos = 0;
      files = [ "src/"; "lib/"; "README.md" ]
     };
  }