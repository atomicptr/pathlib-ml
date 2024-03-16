(** Return a new path object representing the current directory (as returned by Sys.getcwd) *)
let cwd () = Sys.getcwd ()

(** Join a path and a path segment, if you want to apply multiple segments at once check out Pathlib.join_list *)
let join path segment = Filename.concat path segment

(** Join a path with multiple segments at once. *)
let join_list path segments = List.fold_left join path segments

(** Return true if the path points to an existing file or directory. *)
let exists path = Sys.file_exists path

(** Make the path absolute, if its relative prefix it with the current working directory *)
let absolute path = if Filename.is_relative path then join (cwd ()) path else path

(** Is the path a directory? *)
let is_directory path = Sys.is_directory path

(** Is the path a regular file? *)
let is_file path = Sys.is_regular_file path

(** Get the basename of a path: ./test/filename.txt will return filename.txt *)
let basename path = Filename.basename path

(** Get the parent of the path *)
let parent path = Filename.dirname path

(** A list giving access to the path's various components *)
let parts path = List.filter (fun seg -> String.length seg > 0) (String.split_on_char Filename.dir_sep.[0] path)

(** The file extension of the final component *)
let suffix path = Filename.extension path

(** A list of the path’s file extensions *)
let suffixes path = List.tl (List.map (fun ext -> "." ^ ext) (String.split_on_char '.' (Filename.basename path)))

(** The final path component, without its suffix *)
let stem path = Filename.basename path |> String.split_on_char '.' |> List.hd

(** Create directory recursively *)
let rec mkdir path =
  if Sys.file_exists path then ()
  else (
    mkdir (Filename.dirname path);
    Sys.mkdir path 0o755)

(** Write text to file *)
let write file text =
  let out_channel = open_out file in
  Printf.fprintf out_channel "%s" text;
  close_out out_channel

(** Read text from file *)
let read file =
  let in_channel = open_in_bin file in
  let str = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  str

(** Write an empty file to path *)
let touch file = if exists file then read file |> write file else write file ""

(** Return a new path object representing the user’s home directory *)
let home_dir () =
  match Os.current with
  | Os.Linux | Os.MacOS | Os.OtherUnix -> (
      match Sys.getenv_opt "HOME" with
      | Some home_dir -> Ok home_dir
      | None -> Error "HOME environment variable is not set.")
  | Os.Windows -> (
      match Sys.getenv_opt "USERPROFILE" with
      | Some home_dir -> Ok home_dir
      | None -> Error "USERPROFILE environment variable is not set.")
  | _ -> Error "Can't determine home directory for unknown operating system."

(** Get the base location of the users configuration directory *)
let user_config_dir_base () =
  match home_dir () with
  | Ok home_dir -> (
      match Os.current with
      | Linux | OtherUnix -> Ok (join_list home_dir [ ".config" ])
      | MacOS -> Ok (join_list home_dir [ "Library"; "Application Support" ])
      | Windows -> Ok (join_list home_dir [ "AppData"; (if Os.win_is_roaming () then "Roaming" else "Local") ])
      | _ -> Error "Can't determine user config directory for unknown operating system.")
  | Error msg -> Error msg

(** Get the location of the users configuration directory for your app dir *)
let user_config_dir app_name =
  match user_config_dir_base () with
  | Ok config_dir -> Ok (join config_dir app_name)
  | Error msg -> Error msg

(** Get the location of the users cache directory *)
let user_cache_dir app_name =
  match home_dir () with
  | Ok home_dir -> (
      match Os.current with
      | Linux | OtherUnix -> Ok (join_list home_dir [ ".cache"; app_name ])
      | MacOS -> Ok (join_list home_dir [ "Library"; "Caches"; app_name ])
      | Windows ->
          Ok
            (join_list home_dir [ "AppData"; (if Os.win_is_roaming () then "Roaming" else "Local"); app_name; "Cache" ])
      | _ -> Error "Can't determine user cache directory for unknown operating system.")
  | Error msg -> Error msg

(** Get the base temp directory *)
let temp_dir_base () =
  match Os.current with
  | Linux | MacOS | OtherUnix -> (
      match Sys.getenv_opt "TMPDIR" with
      | Some tmp_dir -> Ok tmp_dir
      | None -> Ok "/tmp")
  | Windows -> (
      match Sys.getenv_opt "TEMP" with
      | Some tmp_dir -> Ok tmp_dir
      | None -> Error "TEMP environment variable is not set.")
  | _ -> Error "Can't determine temp directory for unknown operating system."

(** Get a temp directory for your app name *)
let temp_dir app_name =
  match temp_dir_base () with
  | Ok temp_dir -> Ok (join temp_dir app_name)
  | Error msg -> Error msg

(** Make a temp directory for your app name *)
let make_temp_dir app_name =
  match temp_dir app_name with
  | Ok temp_dir ->
      let random_subdir = Utils.make_random_string () in
      let temp_dir = join temp_dir random_subdir in
      mkdir temp_dir;
      Ok temp_dir
  | Error msg -> Error msg

(** Return the relative path from one path to the next *)
let relative_to base_path target_path =
  let rec common_prefix base_lst target_lst =
    match (base_lst, target_lst) with
    | x :: xs, y :: ys when x = y -> common_prefix xs ys
    | _ -> (base_lst, target_lst)
  in
  let common, remaining_target =
    common_prefix
      (String.split_on_char Filename.dir_sep.[0] base_path)
      (String.split_on_char Filename.dir_sep.[0] target_path)
  in
  let ups = List.init (List.length common) (fun _ -> Filename.parent_dir_name) in
  String.concat Filename.dir_sep (ups @ remaining_target)

(** Iterate over every file inside a file tree recursively *)
let rec walk path func =
  Sys.readdir path
  |> Array.iter (fun p ->
         let p = join path p in
         if is_directory p then walk p func else func p)

(** Remove a directory recursively *)
let rec rmdir path =
  Sys.readdir path
  |> Array.iter (fun p ->
         let p = join path p in
         if is_directory p then (
           rmdir p;
           Sys.rmdir p)
         else Unix.unlink p)

(** Rename this file or directory to the given target *)
let rename path new_path = Sys.rename path new_path
