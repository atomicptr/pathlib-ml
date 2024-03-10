type operating_system = Windows | Linux | MacOS | OtherUnix | Unknown

let current =
  match Sys.os_type with
  | "Windows" -> Windows
  | "Unix" -> (
      let in_channel = Unix.open_process_in "uname" in
      let uname = input_line in_channel in
      close_in in_channel;
      match uname with
      | "Linux" -> Linux
      | "Darwin" -> MacOS
      | _ -> OtherUnix)
  | "Cygwin" -> OtherUnix
  | _ -> Unknown

let win_is_roaming () = false
