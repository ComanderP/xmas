open Xmas.Ast
module Lexer = struct end

module Parser = struct
  let empty_program = ("", [])
  let empty_scope = ("{}", [ Expr (Scope []) ])
  let simple_bind = ("value@x", [ Expr (Bind ("x", Var "value")) ])

  let simple_addition =
    ("1 + 2", [ Expr (BinExpr (Add (Literal (Int 1), Literal (Int 2)))) ])

  let simple_negation = ("- (1)", [ Expr (UnaryExpr (Neg (Literal (Int 1)))) ])

  let simple_function_call =
    ("f(1, 2)", [ Expr (Call ("f", [ Literal (Int 1); Literal (Int 2) ])) ])

  let simple_member_call = ("x.y", [ Expr (MemberCall (Var "x", "y", [])) ])
  let variable = ("x", [ Expr (Var "x") ])
  let literal = ("1", [ Expr (Literal (Int 1)) ])
  let simple_fun_def = ("f \\ x y -> {}", [ FunDef ("f", [ "x"; "y" ], []) ])
  let simple_assign = ("x = 1", [ Assign ("x", Literal (Int 1)) ])
  let simple_if = ("if true {}", [ If (Literal (Bool true), []) ])
  let simple_while = ("while true {}", [ While (Literal (Bool true), []) ])

  let simple_for =
    ( "for x in [1, 2] {}",
      [ For ("x", Literal (List [ Literal (Int 1); Literal (Int 2) ]), []) ] )

  let simple_match =
    ( "match x { \\ true -> {} \\ false -> {} }",
      [
        Match
          (Var "x", [ (Literal (Bool true), []); (Literal (Bool false), []) ]);
      ] )
end
