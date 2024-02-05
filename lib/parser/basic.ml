open Angstrom

let ws =
  skip_while (function '\x20' | '\x0a' | '\x0d' | '\x09' -> true | _ -> false)

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
