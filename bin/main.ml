open Xmas

let () =
  let filename =
    if Array.length Sys.argv > 1 then Some Sys.argv.(1) else None
  in
  let ast = Driver.run ?filename () in
  print_endline (Ast.show ast)
