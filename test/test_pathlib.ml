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
  Pathlib.mkdir (Pathlib.join_list test_dir [ "test"; "src"; "sub" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "test"; "src"; "a.ml" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "test"; "src"; "b.ml" ]);
  Pathlib.touch (Pathlib.join_list test_dir [ "test"; "src"; "sub"; "c.ml" ]);
  test_dir

let rec compare_list lst1 lst2 =
  if List.length lst1 <> List.length lst2 then false (* lists have differen length, cant be equal *)
  else if List.length lst1 = 0 && List.length lst2 = 0 then true (* both lists are empty? Thats equal *)
  else if List.hd lst1 = List.hd lst2 then compare_list (List.tl lst1) (List.tl lst2)
  else false

let () =
  print_endline "test: Pathlib.cwd";
  let cwd = Pathlib.cwd () in
  assert (Sys.file_exists cwd)

let () =
  print_endline "test: Pathlib.join_list";
  assert (Pathlib.join_list "test" [ "a"; "b" ] = Pathlib.join (Pathlib.join "test" "a") "b")

let () =
  print_endline "test: Pathlib.parts";
  assert (
    compare_list
      (Pathlib.parts "/home/christopher/ocaml/pathlib-ml/main.ocaml")
      [ "home"; "christopher"; "ocaml"; "pathlib-ml"; "main.ocaml" ])

let () =
  print_endline "test: Pathlib.suffixes";
  assert (compare_list (Pathlib.suffixes "/etc/nginx/something") []);
  assert (compare_list (Pathlib.suffixes "test.txt") [ ".txt" ]);
  assert (compare_list (Pathlib.suffixes "test.tar.gz") [ ".tar"; ".gz" ])

let () =
  print_endline "test: Pathlib.stem";
  assert (Pathlib.stem "test.txt" = "test");
  assert (Pathlib.stem "test.tar.gz" = "test");
  assert (Pathlib.stem "/etc/nginx/something.config" = "something")

let () =
  print_endline "test: Pathlib.posix_path";
  assert (
    Pathlib.posix_path "C:\\\\Users\\OcamlUser\\Projects\\pathlib-ml\\pathlib\\pathlib.ml"
    = "C:/Users/OcamlUser/Projects/pathlib-ml/pathlib/pathlib.ml");
  assert (
    Pathlib.posix_path "/home/ocamluser/projects/pathlib-ml/pathlib/pathlib.ml"
    = "/home/ocamluser/projects/pathlib-ml/pathlib/pathlib.ml")

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
  print_endline "test: Pathlib.write_text/read_text";
  let test_dir = make_test_dir_structure () in
  let test_file = Pathlib.join test_dir "desert.txt" in
  assert (Bool.not (Pathlib.exists test_file));
  Pathlib.write_text test_file "Hello, Camel!";
  assert (Pathlib.exists test_file);
  let text = Pathlib.read_text test_file in
  assert (text = "Hello, Camel!");
  Pathlib.rmdir test_dir

let () =
  print_endline "test: Pathlib.with_name";
  assert (Pathlib.with_name "/usr/local/config.ini" "ocaml.ini" = "/usr/local/ocaml.ini")

let () =
  print_endline "test: Pathlib.with_stem";
  assert (Pathlib.with_stem "/usr/local/config.ini" "ocaml" = "/usr/local/ocaml.ini")

let () =
  print_endline "test: Pathlib.with_suffix";
  assert (Pathlib.with_suffix "/usr/local/config.ini" ".ml" = "/usr/local/config.ml");
  assert (Pathlib.with_suffix "/usr/local/config.ini" "ml" = "/usr/local/config.ml")

let () =
  print_endline "test: Pathlib.match_pattern";
  assert (Pathlib.match_pattern "test.ml" "*.ml");
  assert (Pathlib.match_pattern "/a/b.ml" "**.ml");
  assert (Pathlib.match_pattern "/a/b/c.ml" "**/b/*.ml");
  assert (Bool.not @@ Pathlib.match_pattern "/a/b/c.ml" "a/*.ml");
  (* This should pass: assert (Pathlib.match_pattern "/a.ml" "'/*.ml"); *)
  assert (Bool.not @@ Pathlib.match_pattern "a/b.py" "/*.py")

let () =
  print_endline "test: Pathlib.glob";
  let test_dir = make_test_dir_structure () in
  assert (List.length (Pathlib.glob test_dir "**.ml") = 3);
  assert (List.length (Pathlib.glob test_dir "**/e/test.txt") = 1);
  Pathlib.rmdir test_dir

let () =
  print_endline "test: Pathlib.is_relative_to";
  assert (Pathlib.is_relative_to "/etc/passwd" "/etc");
  assert (Bool.not @@ Pathlib.is_relative_to "/etc/passwd" "/usr")
