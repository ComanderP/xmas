type t = Program of statement list

and statement =
  | FunDef of string * string list * scope
  | Assign of string * expression
  | Expr of expression
  | If of expression * scope
  | While of expression * scope
  | For of string * expression * scope
  | Match of expression * (expression * scope) list

and scope = statement list

and expression =
  | Scope of scope
  | Bind of string * expression
  | BinExpr of bin_expr
  | UnaryExpr of unary_expr
  | Call of string * expression list
  | MemberCall of expression * string * expression list
  | Var of string
  | Literal of base_type

and bin_expr =
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | Exp of expression * expression
  | Eq of expression * expression
  | Neq of expression * expression
  | Lt of expression * expression
  | Le of expression * expression
  | Gt of expression * expression
  | Ge of expression * expression
  | And of expression * expression
  | Or of expression * expression

and unary_expr = Neg of expression | Not of expression

and base_type =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Char of char
  | List of expression list
  | Tuple of expression list
  | Unit
[@@deriving show { with_path = false }]