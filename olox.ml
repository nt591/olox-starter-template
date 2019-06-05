open Scanner
module Olox  = struct
  let run source =
    let tokens = Scanner.scan_tokens source in
    print_int (List.length tokens)

  let run_prompt = fun _ ->
    while true do
      print_string "> ";
      let input = read_line ()
      in run input;
    done

  let run_file filename =
    let channel = open_in filename in
    try
      (* read entire file *)
      let line = really_input_string channel (in_channel_length channel) in
      run line;
      flush stdout;
      close_in channel
    with e ->
      close_in_noerr channel;
      raise e

  let main = fun _ -> match Array.length Sys.argv with
    | 0 -> run_prompt ()
    | 1 -> run_file (Array.get Sys.argv 0)
    | _ -> print_endline "Usage: olox [script]"; exit 64;
end