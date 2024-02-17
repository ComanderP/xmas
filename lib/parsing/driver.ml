module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = ErrorParser.MenhirInterpreter

(** Try to parse the input text [text] with the code-based parser. *)
let fast_parse text =
  let lexbuf = Lexing.from_string text in
  match Parser.program Lexer.read lexbuf with
  | v -> Ok v
  | exception Lexer.Lexical_error msg -> Error (Lexer.Lexical_error msg)
  | exception Parser.Error -> Error Parser.Error

(* ------------------------------------------------------------------------- *)

(* The type of the exceptions that may be raised by the table-based parser. *)
exception Syntax_error of ((int * int) option * string)

(** [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. **)
let env = function I.HandlingError env -> env | _ -> assert false

(** [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)
let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

(** [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions |> E.sanitize |> E.compress
  |> E.shorten 20 (* max width 43 *)

(** [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)
let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> assert false

let succeed _ = assert false

(** [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)
let fail text buffer checkpoint =
  let location = L.range (E.last buffer) in
  let indication =
    Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer)
  in
  let message = ParserMessages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

(** Try to parse the input text [text] with the table-based parser.
   This function does not return a result. We assume that the parser will
   fail. *)
let error_parse filename text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf filename;
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = ErrorParser.Incremental.program lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let parse filename text =
  match fast_parse text with
  | Ok ast -> ast
  | Error (Lexer.Lexical_error _ as err) -> raise err
  | Error _ -> error_parse filename text

let run ?filename () =
  match filename with
  | None -> In_channel.input_all In_channel.stdin |> parse "<UNKNOWN>"
  | Some filename ->
      In_channel.with_open_text filename In_channel.input_all |> parse filename
