let () =
  Alcotest.run "GitHub TUI"
    [ ("FS", Test_fs.tests); ("Pretty", Test_pretty.tests) ]
