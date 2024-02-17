open Position

type t = Program of statement with_pos list

and statement =
  | FunDef of string with_pos * string with_pos list * scope
  | Assign of string with_pos * expression with_pos
  | Expr of expression with_pos
  | If of expression with_pos * scope
  | While of expression with_pos * scope
  | For of string with_pos * expression with_pos * scope
  | Match of expression with_pos * (expression with_pos * scope) list

and scope = statement with_pos list

and expression =
  | Scope of scope
  | Bind of string with_pos * expression with_pos
  | BinExpr of bin_expr with_pos
  | UnaryExpr of unary_expr with_pos
  | Call of string with_pos * expression with_pos list
  | MemberCall of
      expression with_pos * string with_pos * expression with_pos list
  | Var of string with_pos
  | Literal of base_type with_pos

and bin_expr =
  | Add of expression with_pos * expression with_pos
  | Sub of expression with_pos * expression with_pos
  | Mul of expression with_pos * expression with_pos
  | Div of expression with_pos * expression with_pos
  | Mod of expression with_pos * expression with_pos
  | Exp of expression with_pos * expression with_pos
  | Eq of expression with_pos * expression with_pos
  | Neq of expression with_pos * expression with_pos
  | Lt of expression with_pos * expression with_pos
  | Le of expression with_pos * expression with_pos
  | Gt of expression with_pos * expression with_pos
  | Ge of expression with_pos * expression with_pos
  | And of expression with_pos * expression with_pos
  | Or of expression with_pos * expression with_pos

and unary_expr = Neg of expression with_pos | Not of expression with_pos

and base_type =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Char of char
  | List of expression with_pos list
  | Tuple of expression with_pos list
  | Unit
[@@deriving show { with_path = false }]

let ( <$> ) = List.map

let rec strip (Program stmts) = Ast.Program (strip_stmt <$> stmts)

and strip_stmt = function
  | { value = FunDef (name, args, body); _ } ->
      Ast.FunDef (name.value, Position.value <$> args, strip_scope body)
  | { value = Assign (name, expr); _ } ->
      Ast.Assign (name.value, strip_expr expr)
  | { value = Expr expr; _ } -> Ast.Expr (strip_expr expr)
  | { value = If (cond, body); _ } -> Ast.If (strip_expr cond, strip_scope body)
  | { value = While (cond, body); _ } ->
      Ast.While (strip_expr cond, strip_scope body)
  | { value = For (name, expr, body); _ } ->
      Ast.For (name.value, strip_expr expr, strip_scope body)
  | { value = Match (expr, cases); _ } ->
      Ast.Match
        ( strip_expr expr,
          (fun (cond, body) -> (strip_expr cond, strip_scope body)) <$> cases )

and strip_scope stmts = strip_stmt <$> stmts

and strip_expr = function
  | { value = Scope scope; _ } -> Ast.Scope (strip_scope scope)
  | { value = Bind (name, expr); _ } -> Ast.Bind (name.value, strip_expr expr)
  | { value = BinExpr expr; _ } -> Ast.BinExpr (strip_bin_expr expr)
  | { value = UnaryExpr expr; _ } -> Ast.UnaryExpr (strip_unary_expr expr)
  | { value = Call (name, args); _ } ->
      Ast.Call (name.value, strip_expr <$> args)
  | { value = MemberCall (obj, name, args); _ } ->
      Ast.MemberCall (strip_expr obj, name.value, strip_expr <$> args)
  | { value = Var name; _ } -> Ast.Var name.value
  | { value = Literal lit; _ } -> Ast.Literal (strip_base_type lit)

and strip_bin_expr = function
  | { value = Add (lhs, rhs); _ } -> Ast.Add (strip_expr lhs, strip_expr rhs)
  | { value = Sub (lhs, rhs); _ } -> Ast.Sub (strip_expr lhs, strip_expr rhs)
  | { value = Mul (lhs, rhs); _ } -> Ast.Mul (strip_expr lhs, strip_expr rhs)
  | { value = Div (lhs, rhs); _ } -> Ast.Div (strip_expr lhs, strip_expr rhs)
  | { value = Mod (lhs, rhs); _ } -> Ast.Mod (strip_expr lhs, strip_expr rhs)
  | { value = Exp (lhs, rhs); _ } -> Ast.Exp (strip_expr lhs, strip_expr rhs)
  | { value = Eq (lhs, rhs); _ } -> Ast.Eq (strip_expr lhs, strip_expr rhs)
  | { value = Neq (lhs, rhs); _ } -> Ast.Neq (strip_expr lhs, strip_expr rhs)
  | { value = Lt (lhs, rhs); _ } -> Ast.Lt (strip_expr lhs, strip_expr rhs)
  | { value = Le (lhs, rhs); _ } -> Ast.Le (strip_expr lhs, strip_expr rhs)
  | { value = Gt (lhs, rhs); _ } -> Ast.Gt (strip_expr lhs, strip_expr rhs)
  | { value = Ge (lhs, rhs); _ } -> Ast.Ge (strip_expr lhs, strip_expr rhs)
  | { value = And (lhs, rhs); _ } -> Ast.And (strip_expr lhs, strip_expr rhs)
  | { value = Or (lhs, rhs); _ } -> Ast.Or (strip_expr lhs, strip_expr rhs)

and strip_unary_expr = function
  | { value = Neg expr; _ } -> Ast.Neg (strip_expr expr)
  | { value = Not expr; _ } -> Ast.Not (strip_expr expr)

and strip_base_type = function
  | { value = Int i; _ } -> Ast.Int i
  | { value = Bool b; _ } -> Ast.Bool b
  | { value = Float f; _ } -> Ast.Float f
  | { value = String s; _ } -> Ast.String s
  | { value = Char c; _ } -> Ast.Char c
  | { value = List l; _ } -> Ast.List (strip_expr <$> l)
  | { value = Tuple t; _ } -> Ast.Tuple (strip_expr <$> t)
  | { value = Unit; _ } -> Ast.Unit
