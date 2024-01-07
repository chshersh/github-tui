type tab = 
  | Code
  | Issues
  | PullRequests

type t =
  { tab: tab ;
  }

let initial_model: t =
  {
     tab = Code;
  }