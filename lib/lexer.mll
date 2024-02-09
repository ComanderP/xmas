{
  open Parser

  exception BadInput of string

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
      ("match", MATCH);
      ("true", TRUE);
      ("false", FALSE);
    ]
}

let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanum = letter | digit
let id =  letter alphanum*
let int = '-'? digit+
let float = '-'? digit+ '.' digit*
let string = '"' [^'"']* '"'

rule read = parse
  | whitespace { read lexbuf }
  | newline { NEWLINE }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 2)) }
  | id as word { 
      try
        let token = Hashtbl.find keyword_table word in
        token
      with Not_found -> ID word
    }
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
  | '{' { LBRACE}
  | '}' { RBRACE}
  | '[' { LBRACKET}
  | ']' { RBRACKET}
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | "->" { ARROW }
  | '.' { DOT }
  | '@' { AT }
  | '\\' {BACKSLASH}
  (* Anything else is an error *)
  | _ as c { 
      let pos = Lexing.lexeme_start_p lexbuf in
      let msg = Printf.sprintf "unexpected character %c at %s" c (string_of_pos pos) in
      raise (BadInput msg) 
    }
  (* End of file *)
  | eof { EOF }
