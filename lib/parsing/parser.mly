(* Header file for the parser *)
%{
open Position
open AstWithPos

%}
(* Tokens *)
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token <char> CHAR 
%token TRUE
%token FALSE
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token MOD "%"
%token EXP "^"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token EQUALS "="
%token EQ "=="
%token NEQ "!="
%token LT "<"
%token LEQ "<="
%token GT ">"
%token GEQ ">="
%token AND
%token OR
%token NOT
%token IF
// %token ELSE
%token WHILE
%token FOR
%token IN
%token MATCH
// %token HASHTAG "#"
%token COMMA ","
// %token SEMICOLON ";"
%token DOT "."
%token BACKSLASH "\\"
// %token COLON ":" 
%token ARROW "->"
%token AT "@"
%token NEWLINE
%token EOF
(* Associativity and precedence *)
%left AT
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ // TODO: should we have nonassoc for these? (i.e. 1 < 2 < 3 should be a syntax error)
%left PLUS MINUS
%left TIMES DIV MOD
%right EXP
%left NOT
%left DOT


(* Start symbol *)
%start <t> program

(* Grammar rules *)
%%

// Parameterized rule for getting the location of a token easily
%inline located(X) : 
  | x = X; { with_pos x (create $loc) }

let program :=
  | NEWLINE*; EOF; { Program [] }
  | NEWLINE*; prog = statement_list_eof; EOF; { Program prog }

let statement_list_eof :=
  | s = located(statement); NEWLINE*; { [s] }
  | s = located(statement); NEWLINE+; rest = statement_list_eof; { s :: rest }

let statement_list_scope :=
  | s = located(statement); NEWLINE*; { [s] }
  | s = located(statement); NEWLINE+; rest = statement_list_scope; { s :: rest }

(* A statement is either a function definition, a variable assignment, an expression, an if statement, a while loop, a for loop, or a match statement *)
let statement :=
  | f = function_definition; { f }
  | v = variable_assignment; { v }
  | e = located(expression); { Expr e }
  | i = if_statement; { i }
  | w = while_loop; { w }
  | f = for_loop; { f }
  | m = match_statement; { m }

(* A function definition is an identifier followed by a backslash, a list of expressions, an arrow, and a scope *)
let function_definition :=
  | name = located(ID); "\\"; args = located(ID)*; "->"; scope = scope; { FunDef (name, args, scope) }

let if_statement :=
  | IF; expr = located(expression); scope = scope; { If (expr, scope) }
  | IF; expr = located(expression); NEWLINE; scope = scope; { If (expr, scope) }

let while_loop :=
  | WHILE ; expr = located(expression); scope = scope; { While (expr, scope) }
  | WHILE ; expr = located(expression); NEWLINE; scope = scope; { While (expr, scope) }

let for_loop :=
  | FOR ; name = located(ID); IN; expr = located(expression); scope = scope; { For (name, expr, scope) }
  | FOR ; name = located(ID); IN; expr = located(expression); NEWLINE; scope = scope; { For (name, expr, scope) }

let match_statement :=
  | MATCH; expr = located(expression); LBRACE; branches = match_branch+; RBRACE; { Match (expr, branches) } 
  | MATCH; expr = located(expression); NEWLINE; branches = match_branch+; { Match (expr, branches) }

let match_branch :=
  | BACKSLASH; value = located(expression); ARROW; statements = scope; { (value, statements) } 

let scope :=
  | LBRACE; NEWLINE*; RBRACE; { [] }
  | LBRACE; NEWLINE*; s = statement_list_scope; RBRACE; { s }

let variable_assignment := 
  | name = located(ID); "="; expr = located(expression); { Assign (name, expr) } 

let expression := 
  | scope = scope; { Scope scope }
  | expr = located(expression); AT ; name = located(ID); { Bind (name, expr) } 
  | LPAREN; expr = expression; RPAREN; { expr }
  | bin_expr = located(binary_expression); { BinExpr bin_expr } 
  | unary_expr = located(unary_expression); { UnaryExpr unary_expr }
  | name = located(ID); { Var name }
  | name = located(ID); LPAREN; params = separated_list(COMMA, located(expression)); RPAREN; { Call (name, params) }
  | expr = located(expression); DOT; name = located(ID); { MemberCall (expr, name, []) }
  | expr = located(expression); DOT; name = located(ID); LPAREN; params = separated_list(COMMA, located(expression)); RPAREN; { MemberCall (expr, name, params) }
  | value = located(baseType); { Literal value }

let binary_expression := 
  | lhs = located(expression); PLUS; rhs = located(expression); { Add (lhs, rhs) }
  | lhs = located(expression); MINUS; rhs = located(expression); { Sub (lhs, rhs) }
  | lhs = located(expression); TIMES; rhs = located(expression); { Mul (lhs, rhs) }
  | lhs = located(expression); DIV; rhs = located(expression); { Div (lhs, rhs) }
  | lhs = located(expression); MOD; rhs = located(expression); { Mod (lhs, rhs) }
  | lhs = located(expression); EXP; rhs = located(expression); { Exp (lhs, rhs) }
  | lhs = located(expression); EQ; rhs = located(expression); { Eq (lhs, rhs) }
  | lhs = located(expression); NEQ; rhs = located(expression); { Neq (lhs, rhs) }
  | lhs = located(expression); LT; rhs = located(expression); { Lt (lhs, rhs) }
  | lhs = located(expression); LEQ; rhs = located(expression); { Le (lhs, rhs) }
  | lhs = located(expression); GT; rhs = located(expression); { Gt (lhs, rhs) }
  | lhs = located(expression); GEQ; rhs = located(expression); { Ge (lhs, rhs) }
  | lhs = located(expression); AND ; rhs = located(expression); { And (lhs, rhs) }
  | lhs = located(expression); OR ; rhs = located(expression); { Or (lhs, rhs) }
  
let unary_expression := 
  | MINUS; expr = located(expression); { Neg (expr) }
  | NOT; expr = located(expression); { Not (expr) }

let baseType := 
  | var = INT; { Int var } 
  | var = FLOAT; { Float var } 
  | var = STRING; { String var }
  | var = CHAR; { Char var }
  | LBRACKET; vars = separated_list(COMMA, located(expression)); RBRACKET; { List vars } 
  | LPAREN; elem = located(expression)?; COMMA; rest = tuple; { 
    match elem with
    | Some elem -> Tuple (elem :: rest)
    | None -> Tuple rest 
  }
  | TRUE; { Bool true }
  | FALSE; { Bool false }

// This fixes the shift/reduce conflict with RPAREN, but I think the syntax changed a bit
// TODO: Do we allow (,) as an empty tuple? Also, with this rule (,1,2) is also a valid tuple that is equivalent to (1,2) or (,1,2,) or (1,2,)
let tuple :=
  | RPAREN; { [] }
  | elem = located(expression); RPAREN; { [elem] }
  | elem = located(expression); COMMA; rest = tuple; { elem :: rest }
