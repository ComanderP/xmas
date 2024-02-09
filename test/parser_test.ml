open QCheck2
open Xmas
open Xmas.Ast

let ast = Alcotest.testable Ast.pp ( = )

let parse (s : string) : Ast.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

let test_parse (s : string) expected () =
  let res = parse s in
  Alcotest.(check ast) "should parse" (Program expected) res

let empty_program = test_parse "" []
let empty_scope = test_parse "{}" [ Expr (Scope []) ]
let single_bind = test_parse "value@x" [ Expr (Bind ("x", Var "value")) ]

let single_addition =
  test_parse "1 + 2" [ Expr (BinExpr (Add (Literal (Int 1), Literal (Int 2)))) ]

let () =
  let open Alcotest in
  run "Parser"
    [
      ("Simple parsing", [ test_case "empty program" `Quick empty_program ]);
      ( "Simple expressions",
        [
          test_case "empty scope" `Quick empty_scope;
          test_case "single bind" `Quick single_bind;
          test_case "single addition" `Quick single_addition;
        ] );
    ]
