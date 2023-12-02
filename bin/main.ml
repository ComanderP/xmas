open Xmas
open Angstrom

let runParser (str : string) parser =
  match parse_string ~consume:All parser str with
  | Ok v -> v
  | Error msg -> failwith msg

let () =
  let input = "[1 2 3]" in
  let res = runParser input Parser.list in
  List.iter print_int res
