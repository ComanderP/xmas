open Angstrom
open Ast
open Basic

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

let list p = brackets (sep_by (char ' ') (p 0))

let statement expression =
  fix (fun statement ->
      let fun_def =
        (fun f_name args scope -> FunDef (f_name, args, scope))
        <$> variable
        <*> char ':' *> char '\'' *> sep_by (char ' ') variable
        <*> sep_by (char '\n') statement
      in
      let assign =
        (fun var expr -> Assign (var, expr))
        <$> variable
        <*> char '=' *> expression
      in
      let if_ =
        (fun cond scope -> If (cond, scope))
        <$> string "if" *> expression
        <*> braces (many statement)
      in
      let while_ =
        (fun cond scope -> While (cond, scope))
        <$> string "while" *> expression
        <*> braces (many statement)
      in
      let for_ =
        (fun var expr scope -> For (var, expr, scope))
        <$> string "for" *> variable
        <*> string "in" *> expression
        <*> braces (many statement)
      in
      let case_ =
        (fun pattern scope -> (pattern, scope))
        <$> expression
        <*> braces (many statement)
      in
      let match_ =
        (fun expr cases -> Match (expr, cases))
        <$> string "match" *> expression
        <*> braces (many case_)
      in
      fun_def <|> assign <|> if_ <|> while_ <|> for_ <|> match_ <?> "statement")

(* let a b = fix (fun a -> (* definition of `a` in terms of `a` and `b` *))

   let b =
     fix (fun b ->
         let a = a b in
         (* definition of b in terms of `a` and `b` *))

   let a = a b *)

let bin_op = function
  | '+' -> add
  | '-' -> sub
  | '*' -> mul
  | '/' -> div
  | '%' -> mod_
  | _ -> failwith "Invalid operator"

let expression =
  fix (fun expression ->
      let statement = statement expression in
      let scope =
        (fun statements -> Scope statements) <$> braces (many statement)
      in
      let bind =
        (fun expr var -> Bind (var, expr))
        <$> expression
        <*> char '@' *> variable
      in
      let rec prec = function
        | 0 -> chainl1 (prec 1) (expr or_ "or")
        | 1 -> chainl1 (prec 2) (expr and_ "and")
        | 2 -> chainl1 (prec 3) (expr eq "==" <|> expr neq "!=")
        | 3 ->
            chainl1 (prec 4)
              (expr lt "<" <|> expr gt ">" <|> expr le ">=" <|> expr ge ">=")
        | 4 -> chainl1 (prec 5) (expr add "+" <|> expr sub "-")
        | 5 -> chainl1 (prec 6) (expr mul "*" <|> expr div "/")
        | 6 ->
            expr1 (fun x -> Neg x) "-"
            <*> prec 7
            <|> (expr1 (fun x -> Not x) "not" <*> prec 7)
            <|> prec 7
        | 7 ->
            (* Function call *)
            (fun x y -> Call (x, y))
            <$> variable
            <*> sep_by (char ',') (prec 0)
            <|> ((fun x -> Var x) <$> variable)
            <|> ((fun x -> Literal x) <$> baseType)
            <|> parens (prec 0)
            <|> scope <|> bind
        | _ -> failwith "unprecedented precendece level"
      in
      assert false)

(* choice [ (fun x -> Expr x) <$> expr_parse 0 ]) *)
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
