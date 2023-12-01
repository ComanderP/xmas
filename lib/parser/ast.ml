type baseType =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Float of float
  | List of baseType list
  | Function of string
  | Unit

type 'a unaryOp = Neg of 'a | Not of 'a

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

type scope = (string * baseType) list

type expression =
  | Scope of scope
  | Bind of string * expression
  | BinExpr of expression binOp
  | UnaryExpr of expression unaryOp
  | Call of string * expression list
  | Var of string
  | Literal of baseType

type statement =
  | FunDef of string * string list * statement list
  | Assign of string * expression
  | Expr of expression
  | If of expression * statement list
  | Match of expression * (expression * statement list) list
  | While of expression * statement list
  | For of string * expression * statement list

type ast = Program of statement list
