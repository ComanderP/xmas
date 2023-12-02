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

(* TODO: make it accept expression instead*)
let unaryOp =
  char '-' *> integer
  >>| (fun x -> Neg x)
  <|> (string "not" *> integer >>| fun x -> Not x)

let stringParser : string t =
  quotes (take_while (function '"' -> false | _ -> true))

let expr op opstring = string opstring *> return (fun x y -> BinExpr (op x y))
let expr1 op opstring = string opstring *> return (fun x -> UnaryExpr (op x))

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

let statement_parse =
  fix (fun statement_parse ->
      let scope_parse = braces (many statement_parse) in
      let rec functioncall_parse =
        (fun x y -> Call (x, y))
        <$> variable
        <*> sep_by (char ',') (expr_parse 0)
      in
      let rec expr_parse = function
        | 0 -> chainl1 (expr_parse 1) (expr or_ "or")
        | 1 -> chainl1 (expr_parse 2) (expr and_ "and")
        | 2 -> chainl1 (expr_parse 3) (expr eq "==" <|> expr neq "!=")
        | 3 ->
            chainl1 (expr_parse 4)
              (expr lt "<" <|> expr gt ">" <|> expr le ">=" <|> expr ge ">=")
        | 4 -> chainl1 (expr_parse 5) (expr add "+" <|> expr sub "-")
        | 5 -> chainl1 (expr_parse 6) (expr mul "*" <|> expr div "/")
        | 6 ->
            expr1 neg "-" <*> expr_parse 7
            <|> (expr1 not "not" <*> expr_parse 7)
            <|> expr_parse 7
        | 7 ->
            functioncall_parse
            <|> ((fun x -> Var x) <$> variable)
            <|> ((fun x -> Literal x) <$> Lazy.force literal_parse)
            <|> parens (expr_parse 0)
            <|> ((fun x -> Scope x) <$> braces scope_parse)
        | _ -> failwith "unprecedented precendece level"
      in
      choice [ (fun x -> Expr x) <$> expr_parse 0 ])
(*
and scope_parse = lazy (braces (many (Lazy.force statement_parse)))

and expr_parse : int -> expression t = function
  | 0 -> chainl1 (expr_parse 1) (expr or_ "or")
  | 1 -> chainl1 (expr_parse 2) (expr and_ "and")
  | 2 -> chainl1 (expr_parse 3) (expr eq "==" <|> expr neq "!=")
  | 3 ->
      chainl1 (expr_parse 4)
        (expr lt "<" <|> expr gt ">" <|> expr le ">=" <|> expr ge ">=")
  | 4 -> chainl1 (expr_parse 5) (expr add "+" <|> expr sub "-")
  | 5 -> chainl1 (expr_parse 6) (expr mul "*" <|> expr div "/")
  | 6 ->
      expr1 neg "-" <*> expr_parse 7
      <|> (expr1 not "not" <*> expr_parse 7)
      <|> expr_parse 7
  | 7 ->
      Lazy.force functioncall_parse
      <|> ((fun x -> Var x) <$> variable)
      <|> ((fun x -> Literal x) <$> Lazy.force literal_parse)
      <|> parens (expr_parse 0)
  | _ -> failwith "unprecedented precendece level"

and functioncall_parse =
  lazy
    ((fun x y -> Call (x, y)) <$> variable <*> sep_by (char ',') (expr_parse 0))

and literal_parse =
  lazy
    ((fun x -> Int x)
    <$> integer
    <|> ((fun x -> Bool x)
        <$> (string "true" *> return true <|> string "false" *> return false))
    <|> ((fun x -> String x) <$> stringParser)
    <|> ((fun x -> Char x) <$> (char '\'' *> any_char <* char '\''))
    <|> ((fun x -> List x) <$> Lazy.force list)
    <|> char '(' *> char ')' *> return Unit)

and list = lazy (brackets (sep_by (char ' ') (expr_parse 0)))
    *)
