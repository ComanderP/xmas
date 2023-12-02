open Xmas
open Angstrom

let runParser (str : string) parser =
  match parse_string ~consume:All parser str with
  | Ok v -> v
  | Error msg -> failwith msg

let () =
  let input = "a= 1 + 2" in
  let res = runParser input (Lazy.force Parser.statement_parse) in
  print_endline (Ast.show_statement res)
(* List.iter print_int res*)
