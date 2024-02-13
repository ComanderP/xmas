open Xmas

let () =
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
  print_endline (Ast.show ast)
