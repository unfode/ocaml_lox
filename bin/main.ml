open Lib.Interpreter
open Lib.Utility

let () = (
  if (Array.length Sys.argv) != 2 then
    print_endline "Usage: ocaml_lox <file path>"
  else (
    match read_file Sys.argv.(1) with
    | Error e -> print_endline (Printexc.to_string e)
    | Ok source -> (
      match scan source with
      | Error -> print_endline "an error was found during scanning"
      | Correct tokens -> (
        match parse tokens with
        | Error -> print_endline "an error was found during parsing"
        | Correct statements -> (
          match execute_statements statements with
          | Error -> print_endline "an error was found during interpreting"
          | Correct -> ()
        )
      )
    )
  )
);

