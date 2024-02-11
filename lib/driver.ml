exception SyntaxError of (int * int) option * string

open Lexing
module I = Parser.MenhirInterpreter
