open Xmas

let () =
  let args = Sys.argv in
  let input = args.(1) in
  let res = Parser.eval input in
  print_endline (string_of_int res)
