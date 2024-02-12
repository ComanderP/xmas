open Xmas

let parse (s : string) : Ast.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

(* let () =
   let text = In_channel.input_all stdin in
   let res = parse text in
   print_endline (Ast.show res) *)

let () =
  let text = In_channel.input_all stdin in
  let res = Driver.fast_parse text in
  let ast =
    match res with
    | Ok ast -> ast
    | Error err -> (
        match err with
        | Lexer.LexicalError msg -> failwith msg
        | _ -> Driver.parse text)
  in
  print_endline (Ast.show ast)
