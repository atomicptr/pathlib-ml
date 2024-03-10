let make_test_dir_structure () =
  let test_dir =
    match Pathlib.make_temp_dir "pathlib-ml-testdir" with
    | Ok dir -> dir
    | Error msg -> failwith (Printf.sprintf "Failed to create temp dir: %s" msg)
  in
  Pathlib.mkdir (Pathlib.join_list test_dir [ "a"; "b"; "c"; "d"; "e" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "a"; "test.txt" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "a"; "b"; "test.txt" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "a"; "b"; "c"; "test.txt" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "a"; "b"; "c"; "d"; "test.txt" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "a"; "b"; "c"; "d"; "e"; "test.txt" ]);
  test_dir

let () =
  print_endline "test: Pathlib.cwd";
  let cwd = Pathlib.cwd () in
  assert (Sys.file_exists cwd)

let () =
  print_endline "test: Pathlib.join_list";
  assert (Pathlib.join_list "test" [ "a"; "b" ] = Pathlib.join (Pathlib.join "test" "a") "b")

let () =
  print_endline "test: Pathlib.home_dir returns Ok";
  let homedir = Pathlib.home_dir () in
  assert (Result.is_ok homedir);
  assert (Sys.file_exists (Result.get_ok homedir))

let () =
  print_endline "test: Pathlib.user_config_dir returns Ok";
  let config_dir = Pathlib.user_config_dir "test-app" in
  assert (Result.is_ok config_dir)

let () =
  print_endline "test: Pathlib.user_cache_dir returns Ok";
  let config_dir = Pathlib.user_config_dir "test-app" in
  assert (Result.is_ok config_dir)

let () =
  print_endline "test: Pathlib.temp_dir returns Ok";
  let config_dir = Pathlib.temp_dir "test-app" in
  assert (Result.is_ok config_dir)

let () =
  print_endline "test: Pathlib.relative_to";
  let test_dir = make_test_dir_structure () in
  let test_dir_a = Pathlib.join test_dir "a" in
  let test_file_a = Pathlib.join test_dir_a "test.txt" in
  let test_dir_b = Pathlib.join test_dir_a "b" in
  let test_file_b = Pathlib.join test_dir_b "test.txt" in
  let test_dir_c = Pathlib.join test_dir_b "c" in
  assert (Pathlib.relative_to test_dir test_file_a = Pathlib.join "a" "test.txt");
  assert (Pathlib.relative_to test_dir_a test_file_b = Pathlib.join "b" "test.txt");
  assert (Pathlib.relative_to test_dir_c test_file_a = Pathlib.join_list ".." [ ".."; "test.txt" ]);
  Pathlib.rmdir test_dir

let () =
  print_endline "test: Pathlib.walk";
  let test_dir = make_test_dir_structure () in
  let counter = ref 0 in
  Pathlib.walk (Pathlib.join test_dir "a") (fun filename ->
      assert (Pathlib.basename filename = "test.txt");
      incr counter);
  (* there are 5x test.txt defined *)
  assert (counter = ref 5);
  Pathlib.rmdir test_dir

let () =
  print_endline "test: Pathlib.write/read";
  let test_dir = make_test_dir_structure () in
  let test_file = Pathlib.join test_dir "desert.txt" in
  assert (Bool.not (Pathlib.exists test_file));
  Pathlib.write test_file "Hello, Camel!";
  assert (Pathlib.exists test_file);
  let text = Pathlib.read test_file in
  assert (text = "Hello, Camel!");
  Pathlib.rmdir test_dir
