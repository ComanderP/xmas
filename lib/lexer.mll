{
  open Parser

  exception LexicalError of string

  let string_of_pos (p : Lexing.position) : string =
    Printf.sprintf "line %d, characters %d-%d"
      p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol + 1)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table =
    create_hashtable 8 [
      ("if", IF);
      ("while", WHILE);
      ("for", FOR);
      ("in", IN);
      ("match", MATCH);
      ("true", TRUE);
      ("false", FALSE);
    ]

  let string_buff = Buffer.create 256

  let char_for_backslash = function
    | 'b' -> '\008'
    | 't' -> '\009'
    | 'n' -> '\010'
    | 'r' -> '\013'
    | c   -> c

  let cnt = ref 0
}

let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let number = (digit | '_')+ 
let alphanum = letter | digit
let id =  (letter | '_') (alphanum | '_')*
let int = '-'? number
let float = '-'? number '.' (digit | '_')* 
let string = '"' [^'"']* '"'
let comment = ';' [^'\n']*


let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule read = parse
  | whitespace { read lexbuf }
  | comment { read lexbuf }
  | newline { Lexing.new_line lexbuf; NEWLINE }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id as word { 
      try
        let token = Hashtbl.find keyword_table word in
        token
      with Not_found -> ID word
    }
  | '"' { string lexbuf }
  | '\'' {string2 lexbuf }
  (* Basic operators *)
  | '=' { EQUALS }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '^' { EXP }
  | '%' { MOD }
  (* Comparison operators *)
  | "==" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LEQ }
  | ">" { GT }
  | ">=" { GEQ }
  (* Logical operators *)
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  (* Punctuation *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  (* | ';' { SEMICOLON } *)
  (* | ':' { COLON } *)
  | "->" { ARROW }
  | '.' { DOT }
  | '@' { AT }
  | '\\' { BACKSLASH }
  (* Anything else is an error *)
  | _ as c { 
      let pos = Lexing.lexeme_start_p lexbuf in
      let msg = Printf.sprintf "Unexpected character %c at %s" c (string_of_pos pos) in
      raise (LexicalError msg) 
    }
  (* End of file *)
  | eof { EOF }
and string = parse
  | '"' { 
    let str = Buffer.contents string_buff in
    if String.length str = 1
    then CHAR str.[0]
    else STRING str
    }
  | '\\' (backslash_escapes as c) {
    Buffer.add_char string_buff (char_for_backslash c);
    string lexbuf }
  | _ as c { 
    Buffer.add_char string_buff c;
    string lexbuf }
  | eof { raise (LexicalError "Unterminated string. Expected '\"'") }
and string2 = parse
  | '\'' {
    let str = Buffer.contents string_buff in
    if String.length str = 1
    then CHAR str.[0]
    else STRING str
  }
  | '\\' (backslash_escapes as c) {
    Buffer.add_char string_buff (char_for_backslash c);
    string2 lexbuf }
  | _ as c { 
    Buffer.add_char string_buff c;
    string2 lexbuf }
  | eof { raise (LexicalError "Unterminated string. Expected \"'\"") }
