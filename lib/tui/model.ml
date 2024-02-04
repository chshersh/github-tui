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
              [| Dir ("src/", [|
                  File "extra.ml";
                  File "zoo.ml"; |]);
                 Dir ("lib/", [|
                  File "fs.ml";
                  File "fs.mli";
                  File "tui.ml";
                  File "tui.mli";
                  File "cli.ml";
                  File "cli.mli";
                  File "list.ml";
                  File "list.mli"; |]);
                 Dir ("configs/", [||]);
                 Dir ("docs/", [|
                  File "how-to.md";
                  File "troubleshooting.md" |]);
                 Dir ("app/", [|
                  File "main.ml" |]);
                 File "README.md";
              |]
          }
        }
     };
  }
