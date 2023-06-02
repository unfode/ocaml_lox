open Lib.Interpreter

(* ============================= Source ============================== *)

(* ============================= Utilities ============================== *)
let read_file (filename: string) : string = (
  let channel = open_in filename in
  try
    let contents = really_input_string channel (in_channel_length channel) in
    close_in channel;
    contents
  with e ->
    close_in channel;
    raise e
)

let source: string = read_file "./lox-scripts/hof.lox";;

let rec print_token_list (tokens: token_t list) = (
  match tokens with
  | [] -> ()
  | token :: rest_tokens ->
    print_string (token_to_string token);
    print_string " ";
    print_token_list rest_tokens
)

(* ============================= Main ============================== *)
let () = print_newline ();;
let () = (
  match scan source with
  | Error -> print_endline "an error was found during scanning"
  | Correct tokens -> (
    print_token_list tokens;
    print_newline ();
    match parse tokens with
    | Error -> print_endline "an error was found during parsing"
    | Correct statements -> (
      match execute_statements statements with
      | Error -> print_endline "an error was found during interpreting"
      | Correct -> ()
    )
  )
);;

let () = print_newline ();;
