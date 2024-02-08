{
open Parser2
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = parse
| white { read lexbuf }
| if { IF } 
| while { WHILE } 
| match { MATCH }
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| id { ID (Lexing.lexeme lexbuf) }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE}
| '}' { RBRACE}
| "->" { ARROW }
| '\\' {BACKSLASH}
| eof { EOF }
