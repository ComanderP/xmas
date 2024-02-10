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
          test_case "/ -> DIV" `Quick (scan_passes "/" DIV);
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
      ( "integers",
        [
          test_case "0 -> INT 0" `Quick (scan_passes "0" (INT 0));
          test_case "1 -> INT 1" `Quick (scan_passes "1" (INT 1));
          test_case "123 -> INT 123" `Quick (scan_passes "123" (INT 123));
        ] );
      ( "floats",
        [
          test_case "0.0" `Quick (scan_passes "0.0" (FLOAT 0.0));
          test_case "-1.0" `Quick (scan_passes "-1.0" (FLOAT (-1.0)));
          test_case "123.456" `Quick (scan_passes "123.456" (FLOAT 123.456));
          test_case "360." `Quick (scan_passes "360." (FLOAT 360.));
          test_case "123.456_789" `Quick
            (scan_passes "123.456_789" (FLOAT 123.456789));
        ] );
      ( "identifiers",
        [
          test_case "x -> ID x" `Quick (scan_passes "x" (ID "x"));
          test_case "x1 -> ID x1" `Quick (scan_passes "x1" (ID "x1"));
          test_case "x_ -> ID x_" `Quick (scan_passes "x_" (ID "x_"));
          test_case "x_1 -> ID x_1" `Quick (scan_passes "x_1" (ID "x_1"));
        ] );
      ( "whitespace",
        [
          test_case " " `Quick (scan_passes " " EOF);
          test_case "\t" `Quick (scan_passes "\t" EOF);
        ] );
      ( "strings",
        [
          test_case "\"hello\"" `Quick
            (scan_passes "\"hello\"" (STRING "hello"));
        ] );
      ( "keywords",
        [
          test_case "if -> IF" `Quick (scan_passes "if" IF);
          test_case "while -> WHILE" `Quick (scan_passes "while" WHILE);
          test_case "for -> FOR" `Quick (scan_passes "for" FOR);
          test_case "in -> IN" `Quick (scan_passes "in" IN);
          test_case "match -> MATCH" `Quick (scan_passes "match" MATCH);
        ] );
    ]
