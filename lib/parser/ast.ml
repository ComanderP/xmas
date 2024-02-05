type 'a unaryOp = Neg of 'a | Not of 'a [@@deriving variants, show]

type 'a binOp =
  | Add of 'a * 'a
  | Sub of 'a * 'a
  | Mul of 'a * 'a
  | Div of 'a * 'a
  | Mod of 'a * 'a
  | Eq of 'a * 'a
  | Neq of 'a * 'a
  | Lt of 'a * 'a
  | Le of 'a * 'a
  | Gt of 'a * 'a
  | Ge of 'a * 'a
  | And of 'a * 'a
  | Or of 'a * 'a
[@@deriving variants, show]

type baseType =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | List of expression list
  | Unit

and statement =
  | FunDef of string * string list * scope
  | Assign of string * expression
  | Expr of expression
  | If of expression * scope
  | While of expression * scope
  | For of string * expression * scope
  | Match of expression * (expression * statement list) list

and scope = statement list

and expression =
  | Scope of scope
  | Bind of string * expression
  | BinExpr of expression binOp
  | UnaryExpr of expression unaryOp
  | Call of string * expression list
  | Var of string
  | Literal of baseType
[@@deriving show]

type ast = Program of statement list
