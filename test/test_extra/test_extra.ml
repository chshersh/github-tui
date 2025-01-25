(* Utilities for Alcotest *)

module Testable = Testable

let test : string -> (unit -> unit) -> unit Alcotest.test_case =
 fun name test -> (name, `Quick, test)
