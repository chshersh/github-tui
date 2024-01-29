type tab = 
  | Code
  | Issues
  | PullRequests

type t =
  { repo: string ;
    tab: tab ;
    files: string list ;
  }

let initial_model repo: t =
  {
     repo ;
     tab = Code;
     files = [ "src/"; "lib/"; "README.md" ]
  }