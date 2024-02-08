open Xmas
open Ast

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser2.program Lexer.read lexbuf in
  ast

let () =
  let input = "1 + 2" in
  let res = parse input in
  print_endline (Ast.show_ast res)
