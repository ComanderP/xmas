open Lexing
open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* This part concerns the code-based parser [Parser]. *)

(** The function call [attempt1 filename] returns normally only if a syntax
      error has occurred while parsing [filename]. In that case, it returns the
      content of the file. **)
let fast_parse filename =
  let _, lexbuf = L.read filename in
  match Parser.program Lexer.read lexbuf with
  | v -> Ok v
  | exception Lexer.LexicalError msg -> Error (Lexer.LexicalError msg)
  | exception Parser.Error -> Error Parser.Error

(* -------------------------------------------------------------------------- *)

(* This part concerns the table-based parser [ErrorParser]. *)

module I = ErrorParser.MenhirInterpreter

(** [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. **)
let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

(** [state checkpoint] extracts the number of the current state out of a
   checkpoint. **)
let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

(** [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. **)
let show text positions =
  E.extract text positions |> E.sanitize |> E.compress
  |> E.shorten 20 (* max width 43 *)

(** [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. **)
let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

let succeed _ = assert false

(** [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. **)
let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message;
  exit 1

(** [attempt2 filename text] runs the parser. **)
let parse filename =
  (* Allocate and initialize a lexing buffer. *)
  let text, lexbuf = L.read filename in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = ErrorParser.Incremental.program lexbuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  I.loop_handle succeed (fail text buffer) supplier checkpoint

exception SyntaxError of ((int * int) option * string)

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) -> "Invalid syntax"
  | (lazy (Cons (I.Element (state, _, _, _), _))) -> (
      try ParserMessages.message (I.number state)
      with Not_found -> "invalid syntax (no specific message for this eror)")

let rec parse lexbuf (checkpoint : _ I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.read lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (SyntaxError (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
      raise (SyntaxError (None, "invalid syntax (parser rejected the input)"))
