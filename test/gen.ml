(*****************************************************************************)
(*                                                                           *)
(*                      QCheck2 Generators for Ast.t                         *)
(*                                                                           *)
(*****************************************************************************)

open QCheck2.Gen
open Xmas.Ast

let num_char = char_range '0' '9'
let alpha_char = char_range 'a' 'z'
let big_alpha_char = char_range 'A' 'Z'
let alphanumeric = oneof [ num_char; alpha_char; big_alpha_char ]
let num_string = string_size (1 -- 10) ~gen:num_char
let small_string = string_size (1 -- 10) ~gen:alphanumeric

(******************************** BaseTypes **********************************)

let _int = int >|= fun x -> Int x
let _string = small_string >|= fun x -> String x
let _bool = bool >|= fun x -> Bool x
let _float = float >|= fun x -> Float x
let _char = char >|= fun x -> Char x
let _unit = return Unit
let basetype = oneof [ _int; _string; _bool; _float; _char; _unit ]

(********************************* Expressions *******************************)

let scope stmt = list_size (1 -- 10) stmt >|= fun x -> Scope x
let bind expr = pair small_string expr >|= fun (x, y) -> Bind (x, y)
let bin_expr b_op = b_op >|= fun x -> BinExpr x
let un_expr u_op = u_op >|= fun x -> UnaryExpr x

let call expr =
  pair small_string (list_size (1 -- 3) expr) >|= fun (x, y) -> Call (x, y)

let member_call expr =
  tup3 expr small_string (list_size (1 -- 3) expr) >|= fun (x, y, z) ->
  MemberCall (x, y, z)

let var = small_string >|= fun x -> Var x
let literal = basetype >|= fun x -> Literal x

let expression stmt b_op u_op =
  sized
  @@ fix (fun self n ->
         match n with
         | 0 -> oneof [ literal; var; call (self 0); member_call (self 0) ]
         | n ->
             frequency
               [
                 (1, literal);
                 (1, var);
                 (1, call (self (n / 2)));
                 (1, member_call (self (n / 2)));
                 (1, bind (self (n / 2)));
                 (1, bin_expr b_op);
                 (1, un_expr u_op);
                 (1, scope stmt);
               ])

(********************************* Operators **********************************)

let add expr = pair expr expr >|= fun (x, y) -> Add (x, y)
let sub expr = pair expr expr >|= fun (x, y) -> Sub (x, y)
let mul expr = pair expr expr >|= fun (x, y) -> Mul (x, y)
let div expr = pair expr expr >|= fun (x, y) -> Div (x, y)
let _mod expr = pair expr expr >|= fun (x, y) -> Mod (x, y)
let eq expr = pair expr expr >|= fun (x, y) -> Eq (x, y)
let neq expr = pair expr expr >|= fun (x, y) -> Neq (x, y)
let lt expr = pair expr expr >|= fun (x, y) -> Lt (x, y)
let le expr = pair expr expr >|= fun (x, y) -> Le (x, y)
let gt expr = pair expr expr >|= fun (x, y) -> Gt (x, y)
let ge expr = pair expr expr >|= fun (x, y) -> Ge (x, y)
let _and expr = pair expr expr >|= fun (x, y) -> And (x, y)
let _or expr = pair expr expr >|= fun (x, y) -> Or (x, y)
let _not expr = expr >|= fun x -> Not x
let _neg expr = expr >|= fun x -> Neg x

let bin_op =
  let expr = oneof [ var; literal ] in
  oneof
    [
      add expr;
      sub expr;
      mul expr;
      div expr;
      _mod expr;
      eq expr;
      neq expr;
      lt expr;
      le expr;
      gt expr;
      ge expr;
      _and expr;
      _or expr;
    ]

let un_op =
  let expr = oneof [ var; literal ] in
  oneof [ _not expr; _neg expr ]

(********************************* Statements *********************************)

let fun_def stmt =
  tup3 small_string (list_size (1 -- 3) small_string) (list_size (1 -- 10) stmt)
  >|= fun (x, y, z) -> FunDef (x, y, z)

let assign expr = pair small_string expr >|= fun (x, y) -> Assign (x, y)
let expr_stmt expr = expr >|= fun x -> Expr x
let _if expr stmt = pair expr (list_size (1 -- 3) stmt) >|= fun (x, y) -> If (x, y)

let _while expr stmt =
  pair expr (list_size (1 -- 3) stmt) >|= fun (x, y) -> While (x, y)

let _for expr stmt =
  tup3 small_string expr (list_size (1 -- 3) stmt) >|= fun (x, y, z) ->
  For (x, y, z)

let _match expr stmt =
  pair expr (list_size (1 -- 3) (pair expr (list_size (1 -- 3) stmt)))
  >|= fun (x, y) -> Match (x, y)

let statement expr =
  sized
  @@ fix (fun self n ->
         match n with
         | 0 -> oneof [ expr_stmt expr ]
         | n ->
             frequency
               [
                 (1, expr_stmt expr);
                 (1, fun_def (self (n / 2)));
                 (1, assign expr);
                 (1, expr_stmt expr);
                 (1, _if expr (self (n / 2)));
                 (1, _while expr (self (n / 2)));
                 (1, _for expr (self (n / 2)));
                 (1, _match expr (self (n / 2)));
               ])

(********************************* Generators *********************************)

let rec exprr () = expression (sta ()) bin_op un_op
and sta () = statement (exprr ())

let expression = exprr ()
let statement = sta ()
