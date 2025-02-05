(* Utilities for Alcotest *)

let test : string -> (unit -> unit) -> unit Alcotest.test_case =
 fun name test -> (name, `Quick, test)
