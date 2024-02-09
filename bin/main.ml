open Xmas

let parse (s : string) : Ast.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

let () =
  let input = "\'1\'" in
  let res = parse input in
  print_endline (Ast.show res)
