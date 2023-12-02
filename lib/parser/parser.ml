open Angstrom
open Ast

let integer : int t =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let boolean : bool t = string "true" <|> string "false" >>| bool_of_string

let identifier : string t =
  ( ^ )
  <$> take_while1 (function
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
        | _ -> false)
  <*> take_while (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false)

let parens p = char '(' *> p <* char ')'
let braces p = char '{' *> p <* char '}'
let brackets p = char '[' *> p <* char ']'
let quotes p = char '"' *> p <* char '"' <|> (char '\'' *> p <* char '\'')

let reserved = function
  | "if" | "while" | "for" | "true" | "false" | "in" | "or" | "and" | "not"
  | "return" ->
      true
  | _ -> false

let reserved_op = function
  | "+" | "-" | "*" | "/" | "=" | "<" | ">" | "<=" | ">=" | "==" | "%" | "@" ->
      true
  | _ -> false

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let variable : string t =
  identifier >>= fun name ->
  if reserved name then fail "reserved word"
  else if reserved_op name then fail "reserved operator"
  else return name

let arrow = function "->" -> true | _ -> false

(* TODO: make list accept expressions *)
let list = brackets (sep_by (char ' ') integer)

(* TODO: make it accept expression instead*)
let unaryOp =
  char '-' *> integer
  >>| (fun x -> Neg x)
  <|> (string "not" *> integer >>| fun x -> Not x)

let stringParser : string t =
  quotes (take_while (function '"' -> false | _ -> true))

let expr op opstring = string opstring >>| fun x y z -> BinExpr (op x y z)

let rec expression i : expression t =
  match i with
  (* | 0 -> chainl1 (expression 1) (expr or_ "or") *)
  | _ -> failwith "Invalid value for i"

let baseType : baseType t =
  integer
  >>| (fun x -> Int x)
  <|> (boolean >>| fun x -> Bool x)
  <|> (stringParser >>| fun x -> String x)
  <|> (quotes any_char >>| fun x -> Char x)
(* <|> (list >>| fun x -> List (List.map (fun x -> Literal x) x)) *)

(* let expr : int t =
     fix (fun expr ->
         let factor = parens expr <|> integer in
         let term = chainl1 factor (mul <|> div) in
         chainl1 term (add <|> sub))
*)
