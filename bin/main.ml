open Xmas
open Angstrom

let runParser (str : string) parser =
  match parse_string ~consume:All parser str with
  | Ok v -> v
  | Error msg -> failwith msg

let () =
  let input = "if" in
  let res = runParser input Parser.variableParser in
  print_endline res
