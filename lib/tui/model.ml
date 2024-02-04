type code_tab =
  {
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

let initial_model repo: t =
  {
     repo ;
     current_tab = Code;
     code_tab = {
      fs = 
        {
          parents = [];
          current = {
            pos = 0;
            files = 
              [| Dir ("src/", [||]);
                 Dir ("lib/", [||]);
                 File "README.md";
              |]
          }
        }
     };
  }