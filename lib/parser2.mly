(* Header file for the parser *)
%{
open Ast
%}
(* Tokens *)
%token <int> INT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token TIMES "*"
%token PLUS "+"
%token MINUS "-"
%token DIV "/"
%token MOD "%"
%token EXP "^"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token EQUALS "="
%token FOR
%token IN
%token HASHTAG "#"
%token COMMA ","
%token SEMICOLON ";"
%token DOT "."
%token BACKSLASH "\\"
%token COLON ":"
%token ARROW "->"
%token AT "@"
%token EOF
(* Associativity and precedence *)
%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left MINUS
%left TIMES
%left DIV
(* Start symbol *)
%start <ast> program

(* Grammar rules *)
%%



(* A program is a list of statements *)
let program :=
  | ss = statement*; EOF; { Program (ss) }

(* A statement is either a function definition, a variable assignment, an expression, an if statement, a while loop, a for loop, or a match statement *)
let statement :=
  | f = function_definition; { f }
//   | v = variable_assignment { v }
//   | e = expression { e }
//   | i = if_statement { i }
//   | w = while_loop { w }
//   | f = for_loop { f }
//   | m = match_statement { m }

(* A function definition is an identifier followed by a backslash, a list of expressions, an arrow, and a scope *)
let function_definition :=
  | name = ID; "\\"; args = ID*; "->"; scope = scope; { FunDef (name, args, scope) }

let scope :=
  | "{" ; s = statement*; RBRACE; { s }


