open Ast
module Env = Map.Make (String)

type env = value Env.t

and value =
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VString of string
  | VChar of char
  | VList of value list
  | VTuple of value list
  | VUnit
  | VFunction of string * statement * env
  | VScope of env

let rec eval (env : env) (Program stmts) : unit =
  (* The last statement is the result of the program *)
  eval_stmts env stmts

and eval_stmts env e =
  match e with
  | [] -> ()
  | [ stmt ] ->
      let v, _ = eval_stmt env stmt in
      v
  | stmt :: stmts ->
      let _, env' = eval_stmt env stmt in
      eval_stmts env' stmts

and eval_stmt env e : env =
  match e with Expr e -> eval_expr env e | _ -> failwith "not implemented"

and eval_expr env e : value * env =
  match e with
  (* | Var id -> eval_var env id *)
  | UnaryExpr e -> eval_unary_expr env e
  | BinExpr e -> eval_bin_expr env e
  | Literal l -> eval_base_type env l
  | _ -> failwith "not implemented"

and eval_var env v =
  try (Env.find v env, env) with Not_found -> failwith "unbound variable"

and eval_bin_expr env = function
  | Add (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VInt (i1 + i2)
      | VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
      | _ -> failwith "type error")
  | Sub (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VInt (i1 - i2)
      | VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
      | _ -> failwith "type error")
  | Mul (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VInt (i1 * i2)
      | VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
      | _ -> failwith "type error")
  | Div (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VInt (i1 / i2)
      | VFloat f1, VFloat f2 -> VFloat (f1 /. f2)
      | _ -> failwith "type error")
  | Mod (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VInt (i1 mod i2)
      | _ -> failwith "type error")
  | Exp (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 ->
          VInt (int_of_float (float_of_int i1 ** float_of_int i2))
      | VFloat f1, VFloat f2 -> VFloat (f1 ** f2)
      | _ -> failwith "type error")
  | Eq (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VBool (i1 = i2)
      | VFloat f1, VFloat f2 -> VBool (f1 = f2)
      | VString s1, VString s2 -> VBool (s1 = s2)
      | VChar c1, VChar c2 -> VBool (c1 = c2)
      | VBool b1, VBool b2 -> VBool (b1 = b2)
      | _ -> failwith "type error")
  | Neq (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VBool (i1 <> i2)
      | VFloat f1, VFloat f2 -> VBool (f1 <> f2)
      | VString s1, VString s2 -> VBool (s1 <> s2)
      | VChar c1, VChar c2 -> VBool (c1 <> c2)
      | VBool b1, VBool b2 -> VBool (b1 <> b2)
      | _ -> failwith "type error")
  | Lt (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VBool (i1 < i2)
      | VFloat f1, VFloat f2 -> VBool (f1 < f2)
      | _ -> failwith "type error")
  | Le (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VBool (i1 <= i2)
      | VFloat f1, VFloat f2 -> VBool (f1 <= f2)
      | _ -> failwith "type error")
  | Gt (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VBool (i1 > i2)
      | VFloat f1, VFloat f2 -> VBool (f1 > f2)
      | _ -> failwith "type error")
  | Ge (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VInt i1, VInt i2 -> VBool (i1 >= i2)
      | VFloat f1, VFloat f2 -> VBool (f1 >= f2)
      | _ -> failwith "type error")
  | And (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VBool b1, VBool b2 -> VBool (b1 && b2)
      | _ -> failwith "type error")
  | Or (e1, e2) -> (
      match (eval_expr env e1, eval_expr env e2) with
      | VBool b1, VBool b2 -> VBool (b1 || b2)
      | _ -> failwith "type error")

and eval_unary_expr env = function
  | Neg e -> (
      match eval_expr env e with
      | VInt i -> VInt (-i)
      | VFloat f -> VFloat (-.f)
      | _ -> failwith "type error")
  | Not e -> (
      match eval_expr env e with
      | VBool b -> VBool (not b)
      | _ -> failwith "type error")

and eval_base_type env = function
  | Int i -> VInt i
  | Bool b -> VBool b
  | Float f -> VFloat f
  | String s -> VString s
  | Char c -> VChar c
  | List l -> VList (List.map (eval_expr env) l)
  | Tuple t -> VTuple (List.map (eval_expr env) t)
  | Unit -> VUnit

let interpret text = Driver.parse "" text |> AstWithPos.strip |> eval Env.empty
