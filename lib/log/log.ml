let log ?path ~tag msg =
  match path with
  | None -> ()
  | Some path ->
      let now = Unix.gettimeofday () in
      let str = Printf.sprintf "timestamp=%f tag=%s msg=%s\n" now tag msg in
      Out_channel.(
        with_open_gen [ Open_creat; Open_append; Open_binary ] 0o644 path
          (fun channel -> output_string channel str))
