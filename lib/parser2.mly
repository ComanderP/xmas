(* Header file for the parser *)
%{
open Ast
%}
(* Tokens *)
%token <int> INT
%token <float> FLOAT
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
%token IF
%token WHILE
%token FOR
%token IN
%token MATCH
%token HASHTAG "#"
%token COMMA ","
%token SEMICOLON ";"
%token DOT "."
%token BACKSLASH "\\"
%token COLON ":" 
%token ARROW "->"
%token AT "@"
%token NEWLINE
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
  | v = variable_assignment; { v }
  | e = expression; { Expr (e)  }
  | i = if_statement; { i }  
  | w = while_loop; { w }
  | f = for_loop; { f }
  | m = match_statement; { m }

(* A function definition is an identifier followed by a backslash, a list of expressions, an arrow, and a scope *)
let function_definition :=
  | name = ID; "\\"; args = ID*; "->"; scope = scope; { FunDef (name, args, scope) }

let if_statement :=
  | IF; expr = expression; scope = scope; { If (expr, scope) }

let while_loop :=
  | WHILE ; expr = expression; scope = scope; { While (expr, scope) }

let for_loop :=
  | FOR ; name = ID; IN; expr = expression; scope = scope; { For (name, expr, scope) }

let match_statement :=
  | MATCH; expr = expression; LBRACE; branches = match_branch+; RBRACE; { Match (expr, branches) } 

let match_branch :=
  | BACKSLASH; value = expression; ARROW; statements = statement+; { (value, statements) } 

let scope :=
  | LBRACE ; s = statement*; RBRACE; { s }

let variable_assignment := 
  | name = ID; "="; expr = expression; { Assign (name, expr) } 

let expression := 
  | scope = scope; { Scope (scope) }
  | expr = expression; AT ; name = ID; { Bind(name, expr) } 
  | binOp = binOp; { BinExpr binOp }
  | uniOp = uniOp; { UnaryExpr uniOp }
  | name = ID; { Var (name) }
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN; { Call(name, params) }
  | value = baseType; { Literal (value) }

let binOp := 
  | var1 = expression; PLUS; var2 = expression; { Add (var1, var2) }
  | var1 = expression; MINUS; var2 = expression; { Sub (var1, var2) }

let uniOp := 
  | MINUS; var = expression; { Neg (var) }

let baseType := 
  | var = INT; { Int (var) } 

