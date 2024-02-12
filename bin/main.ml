open Xmas

let parse (s : string) : Ast.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

(* let () =
   let text = In_channel.input_all stdin in
   let res = parse text in
   print_endline (Ast.show res) *)

let parse lexbuf =
  try
    let grammar =
      Driver.parse lexbuf (ErrorParser.Incremental.program lexbuf.lex_curr_p)
    in
    Ok grammar
  with Driver.SyntaxError (pos, err) -> (
    match pos with
    | Some (line, pos) ->
        Error
          (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos
             err)
    | None -> Error (Printf.sprintf "Syntax error: %s" err))

let () =
  let text, lexbuf = MenhirLib.LexerUtil.read Sys.argv.(1) in
  let res = parse lexbuf in
  match res with
  | Error err -> print_endline err
  | Ok ast -> print_endline "Parsed"

(* let () =
   let filename = Sys.argv.(1) in
   let res = Driver.fast_parse filename in
   let ast =
     match res with
     | Ok ast -> ast
     | Error err -> (
         match err with
         | Lexer.LexicalError msg -> failwith msg
         | _ -> Driver.parse filename)
   in
   print_endline (Ast.show ast) *)
