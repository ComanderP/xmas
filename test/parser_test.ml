open Fixtures.Parser
open Xmas

let ast = Alcotest.testable Ast.pp ( = )

let parse (s : string) : Ast.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

let test_parse fix () =
  let res = parse (fst fix) in
  Alcotest.(check ast) "should parse" (Program (snd fix)) res

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "Simple parsing",
        [ test_case "empty program" `Quick (test_parse empty_program) ] );
      ( "Simple expressions",
        [
          test_case "empty scope" `Quick (test_parse empty_scope);
          test_case "single bind" `Quick (test_parse simple_bind);
          test_case "single addition" `Quick (test_parse simple_addition);
          test_case "simple negation" `Quick (test_parse simple_negation);
          test_case "simple function call" `Quick
            (test_parse simple_function_call);
          test_case "simple member call" `Quick (test_parse simple_member_call);
          test_case "simple var" `Quick (test_parse variable);
          test_case "simple literal" `Quick (test_parse literal);
        ] );
      ( "Simple statements",
        [
          test_case "simple function definition" `Quick
            (test_parse simple_fun_def);
          test_case "simple assignment" `Quick (test_parse simple_assign);
          test_case "simple if" `Quick (test_parse simple_if);
          test_case "simple while" `Quick (test_parse simple_while);
          test_case "simple for" `Quick (test_parse simple_for);
          test_case "simple match" `Quick (test_parse simple_match);
        ] );
    ]
