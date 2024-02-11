open Xmas

let parse (s : string) : Ast.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

let () =
  let text = In_channel.input_all stdin in
  let res = parse text in
  print_endline (Ast.show res)
