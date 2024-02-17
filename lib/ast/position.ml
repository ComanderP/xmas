open Lexing

type t = { start_p : position; end_p : position }
type 'a with_pos = { value : 'a; position : t }

let pp_position fmt p =
  Format.fprintf fmt "l:%d,c:%d" p.pos_lnum (p.pos_cnum - p.pos_bol)

let pp_pos fmt p =
  Format.fprintf fmt "@[<hov>Start:@ %a@;End:@ %a@]" pp_position p.start_p
    pp_position p.end_p

let pp_with_pos pp_value fmt { value; position } =
  Format.fprintf fmt "@[<v>%a@;%a@]" pp_value value pp_pos position

let value x = x.value
let position x = x.position
let create (start_p, end_p) = { start_p; end_p }
let line p = p.pos_lnum
let column p = p.pos_cnum - p.pos_bol
let characters p1 p2 = (column p1, p2.pos_cnum - p1.pos_bol)
let with_pos value position = { value; position }
