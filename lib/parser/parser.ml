open Angstrom

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let identifier =
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

let reserved = function
  | "if" | "while" | "for" | "true" | "false" | "in" | "or" | "and" | "not" ->
      true
  | _ -> false

let reserved_op = function
  | "+" | "-" | "*" | "/" | "=" | "<" | ">" | "<=" | ">=" | "==" | "%" | "@" ->
      true
  | _ -> false

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let variableParser =
  identifier >>= fun name ->
  if reserved name then fail "reserved word"
  else if reserved_op name then fail "reserved operator"
  else return name

(* let expr : int t =
     fix (fun expr ->
         let factor = parens expr <|> integer in
         let term = chainl1 factor (mul <|> div) in
         chainl1 term (add <|> sub))
*)
