let test_nested_directory_rule () =
  let rules = Fs.Gitignore.parse "/.github/workflows/\n" in
  Alcotest.(check bool) "directory is ignored" true
    (Fs.Gitignore.is_ignored ".github/workflows/" rules);
  Alcotest.(check bool) "nested file is ignored" true
    (Fs.Gitignore.is_ignored ".github/workflows/ci.yml" rules);
  Alcotest.(check bool) "sibling directory is not ignored" false
    (Fs.Gitignore.is_ignored ".github/" rules)

let test_go_next_skips_empty_filtered_directory () =
  let hidden_dir = Fs.Dir { name = "workflows"; ignored = true; children = lazy [||] } in
  let parent_dir =
    Fs.Dir
      {
        name = ".github";
        ignored = false;
        children = lazy [| hidden_dir |];
      }
  in
  let zipper = Fs.zip_it [| parent_dir |] ~show_ignored:false in
  let moved = Fs.go_next zipper in
  Alcotest.(check int) "does not move to child directory" 0
    (List.length moved.parents);
  match moved.current with
  | Fs.Dir_cursor cursor ->
      Alcotest.(check int) "root cursor remains non-empty" 1
        (Array.length cursor.files)
  | Fs.File_cursor _ -> Alcotest.fail "Expected directory cursor"

let test_move_in_empty_directory_cursor () =
  let hidden_dir = Fs.Dir { name = "hidden"; ignored = true; children = lazy [||] } in
  let zipper = Fs.zip_it [| hidden_dir |] ~show_ignored:false in
  let moved_down = Fs.go_down zipper in
  let moved_up = Fs.go_up zipper in
  let assert_empty = function
    | Fs.Dir_cursor cursor ->
        Alcotest.(check int) "empty cursor remains empty" 0
          (Array.length cursor.files)
    | Fs.File_cursor _ -> Alcotest.fail "Expected directory cursor"
  in
  assert_empty moved_down.current;
  assert_empty moved_up.current

let tests =
  [
    Test_extra.test "nested directory gitignore rule" test_nested_directory_rule;
    Test_extra.test "skip entering filtered empty directory"
      test_go_next_skips_empty_filtered_directory;
    Test_extra.test "safe movement in empty directory cursor"
      test_move_in_empty_directory_cursor;
  ]
