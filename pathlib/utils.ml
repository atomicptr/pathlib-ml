let make_hash input_string = Digest.string input_string |> Digest.to_hex

let make_random_string () =
  Random.self_init ();
  Unix.time () +. Random.float 10000.0 |> string_of_float |> make_hash
