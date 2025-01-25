open Pretty.Color

let check ~name ~expected ~input =
  Alcotest.check Test_extra.Testable.color name expected (of_hex input)

let test_black name =
  let expected = RGB (0, 0, 0) in
  let input = "000000" in
  check ~name ~expected ~input

let test_white name =
  let expected = RGB (255, 255, 255) in
  let input = "ffffff" in
  check ~name ~expected ~input

let test_hex name =
  let expected = RGB (251, 202, 4) in
  let input = "#fbca04" in
  check ~name ~expected ~input

let test name test = Test_extra.test name (fun () -> test name)

let tests =
  [ test "Black" test_black; test "White" test_white; test "Hex" test_hex ]
