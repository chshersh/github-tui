type tab = 
  | Code
  | Issues
  | PullRequests

type t =
  { repo: string ;
    tab: tab ;
  }

let initial_model repo: t =
  {
     repo ;
     tab = Code;
  }