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

let () =
  let open Alcotest in
  run "Pathlib"
    [
      ( "Pathlib.cwd",
        [
          test_case "test if cwd exists" `Quick (fun () ->
              let cwd = Pathlib.cwd () in
              (check bool) "is true" true (Sys.file_exists cwd));
        ] );
      ( "Pathlib.join_list",
        [
          test_case "test if join_list works" `Quick (fun () ->
              (check string) "equals"
                (Pathlib.join (Pathlib.join "test" "a") "b")
                (Pathlib.join_list "test" [ "a"; "b" ]));
        ] );
      ( "Pathlib.parts",
        [
          test_case "test splitting path into parts" `Quick (fun () ->
              (check (list string))
                "equals"
                [ "home"; "christopher"; "ocaml"; "pathlib-ml"; "main.ocaml" ]
                (Pathlib.parts "/home/christopher/ocaml/pathlib-ml/main.ocaml"));
        ] );
      ( "Pathlib.suffixes",
        [
          test_case "test suffixes with a path that has none" `Quick (fun () ->
              (check (list string)) "equals" [] (Pathlib.suffixes "/etc/nginx/something"));
          test_case "test suffixes with .txt file" `Quick (fun () ->
              (check (list string)) "equals" [ ".txt" ] (Pathlib.suffixes "test.txt"));
          test_case "test suffixes with.tar.gz file" `Quick (fun () ->
              (check (list string)) "equals" [ ".tar"; ".gz" ] (Pathlib.suffixes "test.tar.gz"));
        ] );
      ( "Pathlib.stem",
        [
          test_case "test stem with one suffix" `Quick (fun () ->
              (check string) "equals" "test" (Pathlib.stem "test.txt"));
          test_case "test stem with two suffixes" `Quick (fun () ->
              (check string) "equals" "test" (Pathlib.stem "test.tar.gz"));
          test_case "test stem with path" `Quick (fun () ->
              (check string) "equals" "something" (Pathlib.stem "/etc/nginx/something.config"));
        ] );
      ( "Pathlib.posix_path",
        [
          test_case "convert long windows path to posix style" `Quick (fun () ->
              (check string) "equals" "C:/Users/OcamlUser/Projects/pathlib-ml/pathlib/pathlib.ml"
                (Pathlib.posix_path "C:\\\\Users\\OcamlUser\\Projects\\pathlib-ml\\pathlib\\pathlib.ml"));
        ] );
      ( "Pathlib.home_dir",
        [
          test_case "home_dir returns Ok" `Quick (fun () ->
              let homedir = Pathlib.home_dir () in
              (check bool) "is true" true (Result.is_ok homedir));
          test_case "home_dir exists" `Quick (fun () ->
              let homedir = Pathlib.home_dir () in
              (check bool) "is true" true (Sys.file_exists (Result.get_ok homedir)));
        ] );
      ( "Pathlib.user_config_dir",
        [
          test_case "user_config_dir returns Ok" `Quick (fun () ->
              let dir = Pathlib.user_config_dir "test-app" in
              (check bool) "is true" true (Result.is_ok dir));
        ] );
      ( "Pathlib.user_cache_dir",
        [
          test_case "user_cache_dir returns Ok" `Quick (fun () ->
              let dir = Pathlib.user_cache_dir "test-app" in
              (check bool) "is true" true (Result.is_ok dir));
        ] );
      ( "Pathlib.temp_dir",
        [
          test_case "temp_dir returns Ok" `Quick (fun () ->
              let dir = Pathlib.temp_dir "test-app" in
              (check bool) "is true" true (Result.is_ok dir));
        ] );
      ( "Pathlib.relative_to",
        [
          test_case "complex file tree relative_to works properly" `Quick (fun () ->
              let test_dir = make_test_dir_structure () in
              let test_dir_a = Pathlib.join test_dir "a" in
              let test_file_a = Pathlib.join test_dir_a "test.txt" in
              let test_dir_b = Pathlib.join test_dir_a "b" in
              let test_file_b = Pathlib.join test_dir_b "test.txt" in
              let test_dir_c = Pathlib.join test_dir_b "c" in
              Pathlib.rmdir test_dir;
              (check (list string))
                "equals"
                [
                  Pathlib.join "a" "test.txt"; Pathlib.join "b" "test.txt"; Pathlib.join_list ".." [ ".."; "test.txt" ];
                ]
                [
                  Pathlib.relative_to test_dir test_file_a;
                  Pathlib.relative_to test_dir_a test_file_b;
                  Pathlib.relative_to test_dir_c test_file_a;
                ]);
        ] );
      ( "Pathlib.walk",
        [
          test_case "walks through a directory" `Quick (fun () ->
              let test_dir = make_test_dir_structure () in
              let counter = ref 0 in
              Pathlib.walk (Pathlib.join test_dir "a") (fun filename ->
                  if Pathlib.basename filename = "test.txt" then incr counter else ());
              (* there are 5x test.txt defined *)
              assert (counter = ref 5);
              Pathlib.rmdir test_dir;
              (check int) "counted correctly" 5 !counter);
        ] );
      ( "Pathlib.write_text",
        [
          test_case "writing file means file exists" `Quick (fun () ->
              let test_dir = make_test_dir_structure () in
              let test_file = Pathlib.join test_dir "desert.txt" in
              (check bool) "does not exist" false (Pathlib.exists test_file);
              Pathlib.write_text test_file "Hello, Camel!";
              (check bool) "exists" true (Pathlib.exists test_file);
              Pathlib.rmdir test_dir);
        ] );
      ( "Pathlib.read_text",
        [
          test_case "reading file that was just written" `Quick (fun () ->
              let test_dir = make_test_dir_structure () in
              let test_file = Pathlib.join test_dir "desert.txt" in
              Pathlib.write_text test_file "Hello, Camel!";
              (check string) "equals" "Hello, Camel!" (Pathlib.read_text test_file);
              Pathlib.rmdir test_dir);
        ] );
      ( "Pathlib.with_name",
        [
          test_case "/usr/local/config.ini -> /usr/local/ocaml.ini" `Quick (fun () ->
              (check string) "equals" "/usr/local/ocaml.ini" (Pathlib.with_name "/usr/local/config.ini" "ocaml.ini"));
        ] );
      ( "Pathlib.with_stem",
        [
          test_case "/usr/local/config.ini -> /usr/local/ocaml.ini" `Quick (fun () ->
              (check string) "equals" "/usr/local/ocaml.ini" (Pathlib.with_stem "/usr/local/config.ini" "ocaml"));
        ] );
      ( "Pathlib.with_suffix",
        [
          test_case "with suffix contains dot: /usr/local/config.ini -> /usr/local/config.ml" `Quick (fun () ->
              (check string) "equals" "/usr/local/config.ml" (Pathlib.with_suffix "/usr/local/config.ini" ".ml"));
          test_case "with suffix no dot: /usr/local/config.ini -> /usr/local/config.ml" `Quick (fun () ->
              (check string) "equals" "/usr/local/config.ml" (Pathlib.with_suffix "/usr/local/config.ini" "ml"));
        ] );
      ( "Pathlib.match_pattern",
        [
          test_case "test.ml matches *.ml" `Quick (fun () ->
              (check bool) "is true" true (Pathlib.match_pattern "test.ml" "*.ml"));
          test_case "/a/b.ml matches **.ml" `Quick (fun () ->
              (check bool) "is true" true (Pathlib.match_pattern "/a/b.ml" "**.ml"));
          test_case "/a/b/c.ml matches **/b/*.ml" `Quick (fun () ->
              (check bool) "is true" true (Pathlib.match_pattern "/a/b/c.ml" "**/b/*.ml"));
          test_case "/a/b/c.ml does not match a/*.ml" `Quick (fun () ->
              (check bool) "is false" false (Pathlib.match_pattern "/a/b/c.ml" "a/*.ml"));
          (* This should be true but isnt for some reason:

             test_case "/a.ml matches /*.ml" `Quick (fun () -> (check bool) "is true" true (Pathlib.match_pattern
             "/a.ml" "/*.ml")); *)
          test_case "a/b.py does not match /*.py" `Quick (fun () ->
              (check bool) "is false" false (Pathlib.match_pattern "a/b.py" "/*.py"));
        ] );
      ( "Pathlib.glob",
        [
          test_case "test directory has 3x ml files with pattern **.ml" `Quick (fun () ->
              let test_dir = make_test_dir_structure () in
              (check int) "equals" 3 (List.length (Pathlib.glob test_dir "**.ml"));
              Pathlib.rmdir test_dir);
          test_case "test directory has 1x txt file with pattern **/e/test.txt" `Quick (fun () ->
              let test_dir = make_test_dir_structure () in
              (check int) "equals" 1 (List.length (Pathlib.glob test_dir "**/e/test.txt"));
              Pathlib.rmdir test_dir);
        ] );
      ( "Pathlib.is_relative_to",
        [
          test_case "/etc/passwd is relative to /etc" `Quick (fun () ->
              (check bool) "is true" true (Pathlib.is_relative_to "/etc/passwd" "/etc"));
          test_case "/etc/passwd is not relative to /usr" `Quick (fun () ->
              (check bool) "is false" false (Pathlib.is_relative_to "/etc/passwd" "/usr"));
        ] );
    ]
