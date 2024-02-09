open QCheck2
open Xmas
open Xmas.Parser

let scan (s : string) : token =
  let lexbuf = Lexing.from_string s in
  Lexer.read lexbuf

let test_scan (s : string) (t : token) (b : bool) =
  Alcotest.(check bool)
    (Printf.sprintf "Scanning %s works correctly" s)
    b
    (scan s = t)

let scan_passes (s : string) (t : token) () = test_scan s t true

let () =
  let open Alcotest in
  run "Lexer"
    [
      ( "math operators",
        [
          test_case "+ -> PLUS" `Quick (scan_passes "+" PLUS);
          test_case "- -> MINUS" `Quick (scan_passes "-" MINUS);
          test_case "* -> TIMES" `Quick (scan_passes "*" TIMES);
          test_case "/ -> DIVIDE" `Quick (scan_passes "/" DIV);
          test_case "^ -> EXP" `Quick (scan_passes "^" EXP);
          test_case "% -> MOD" `Quick (scan_passes "%" MOD);
        ] );
      ( "comparison operators",
        [
          test_case "< -> LT" `Quick (scan_passes "<" LT);
          test_case "<= -> LTE" `Quick (scan_passes "<=" LEQ);
          test_case "> -> GT" `Quick (scan_passes ">" GT);
          test_case ">= -> GTE" `Quick (scan_passes ">=" GEQ);
          test_case "== -> EQ" `Quick (scan_passes "==" EQ);
          test_case "!= -> NEQ" `Quick (scan_passes "!=" NEQ);
        ] );
      ( "logical operators",
        [
          test_case "&& -> AND" `Quick (scan_passes "&&" AND);
          test_case "|| -> OR" `Quick (scan_passes "||" OR);
          test_case "!" `Quick (scan_passes "!" NOT);
        ] );
      ( "parentheses",
        [
          test_case "( -> LPAREN" `Quick (scan_passes "(" LPAREN);
          test_case ") -> RPAREN" `Quick (scan_passes ")" RPAREN);
        ] );
      ( "booleans",
        [
          test_case "true -> TRUE" `Quick (scan_passes "true" TRUE);
          test_case "false -> FALSE" `Quick (scan_passes "false" FALSE);
        ] );
    ]
