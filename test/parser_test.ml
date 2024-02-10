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
let simple_bind = test_parse "value@x" [ Expr (Bind ("x", Var "value")) ]

let simple_addition =
  test_parse "1 + 2" [ Expr (BinExpr (Add (Literal (Int 1), Literal (Int 2)))) ]

let simple_negation =
  test_parse "- (1)"
    [ Expr (UnaryExpr (Neg (Literal (Int 1) ))) ]

let simple_function_call =
  test_parse "f(1, 2)"
    [ Expr (Call ("f", [ Literal (Int 1); Literal (Int 2) ])) ]

let simple_member_call =
  test_parse "x.y" [ Expr (MemberCall (Var "x", "y", [])) ]

let variable = test_parse "x" [ Expr (Var "x") ]
let literal = test_parse "1" [ Expr (Literal (Int 1)) ]

let simple_fun_def =
  test_parse "f \\ x y -> {}" [ FunDef ("f", [ "x"; "y" ], []) ]

let simple_assign = test_parse "x = 1" [ Assign ("x", Literal (Int 1)) ]
let simple_if = test_parse "if true {}" [ If (Literal (Bool true), []) ]

let simple_while =
  test_parse "while true {}" [ While (Literal (Bool true), []) ]

let simple_for =
  test_parse "for x in [1, 2] {}"
    [ For ("x", Literal (List [ Literal (Int 1); Literal (Int 2) ]), []) ]

let simple_match =
  test_parse "match x { \\ true -> {} \\ false -> {} }"
    [
      Match (Var "x", [ (Literal (Bool true), []); (Literal (Bool false), []) ]);
    ]

let () =
  let open Alcotest in
  run "Parser"
    [
      ("Simple parsing", [ test_case "empty program" `Quick empty_program ]);
      ( "Simple expressions",
        [
          test_case "empty scope" `Quick empty_scope;
          test_case "single bind" `Quick simple_bind;
          test_case "single addition" `Quick simple_addition;
          test_case "simple negation" `Quick simple_negation;
          test_case "simple function call" `Quick simple_function_call;
          test_case "simple member call" `Quick simple_member_call;
          test_case "simple var" `Quick variable;
          test_case "simple literal" `Quick literal;
        ] );
      ( "Simple statements",
        [
          test_case "simple function definition" `Quick simple_fun_def;
          test_case "simple assignment" `Quick simple_assign;
          test_case "simple if" `Quick simple_if;
          test_case "simple while" `Quick simple_while;
          test_case "simple for" `Quick simple_for;
          test_case "simple match" `Quick simple_match;
        ] );
    ]
