(* Header file for the parser *)
%{
open Ast

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

let program :=
  | NEWLINE*; EOF; { Program [] }
  | NEWLINE*; prog = statement_list_eof; EOF; { Program prog }

let statement_list_eof :=
  | s = statement; NEWLINE*; { s::[] }
  | s = statement; NEWLINE+; rest = statement_list_eof; { s :: rest }

let statement_list_scope :=
  | NEWLINE+; statement_list_scope = statement_list_scope; { statement_list_scope }
  | s = statement; RBRACE; { s::[] }
  | s = statement; NEWLINE+; rest = statement_list_scope; { s :: rest }
  | RBRACE; { [] }

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
  | BACKSLASH; value = expression; ARROW; statements = scope; { (value, statements) } 

let scope :=
  | LBRACE ; s = statement_list_scope; { s }

let variable_assignment := 
  | name = ID; "="; expr = expression; { Assign (name, expr) } 

let expression := 
  | scope = scope; { Scope scope }
  | expr = expression; AT ; name = ID; { Bind (name, expr) } 
  | LPAREN; expr = expression; RPAREN; { expr } // FIXME: shift/reduce conflict with basicType tuple
  | binOp = binOp; { BinExpr binOp }
  | uniOp = uniOp; { UnaryExpr uniOp }
  | name = ID; { Var name }
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN; { Call(name, params) }
  | expr = expression; DOT; name = ID; { MemberCall (expr, name, []) }
  | expr = expression; DOT; name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN; { MemberCall (expr, name, params) }
  | value = baseType; { Literal value }

let binOp := 
  | var1 = expression; PLUS; var2 = expression; { Add (var1, var2) }
  | var1 = expression; MINUS; var2 = expression; { Sub (var1, var2) }
  | var1 = expression; TIMES; var2 = expression; { Mul (var1, var2) }
  | var1 = expression; DIV; var2 = expression; { Div (var1, var2) }
  | var1 = expression; MOD; var2 = expression; { Mod (var1, var2) }
  | var1 = expression; EXP; var2 = expression; { Exp (var1, var2) }
  | var1 = expression; EQ; var2 = expression; { Eq (var1, var2) }
  | var1 = expression; NEQ; var2 = expression; { Neq (var1, var2) }
  | var1 = expression; LT; var2 = expression; { Lt (var1, var2) }
  | var1 = expression; LEQ; var2 = expression; { Le (var1, var2) }
  | var1 = expression; GT; var2 = expression; { Gt (var1, var2) }
  | var1 = expression; GEQ; var2 = expression; { Ge (var1, var2) }
  | var1 = expression; AND ; var2 = expression; { And (var1, var2) }
  | var1 = expression; OR ; var2 = expression; { Or (var1, var2) }

let uniOp := 
  | MINUS; var = expression; { Neg (var) }
  | NOT; var = expression; { Not (var) }

let baseType := 
  | var = INT; { Int var } 
  | var = FLOAT; { Float var } 
  | var = STRING; { String var }
  | var = CHAR; { Char var }
  | LBRACKET; vars = separated_list(COMMA, expression); RBRACKET; { List vars } 
  | LPAREN; elem = expression?; COMMA; rest = tuple; { 
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
  | elem = expression; RPAREN; { [elem] }
  | elem = expression; COMMA; rest = tuple; { elem :: rest }