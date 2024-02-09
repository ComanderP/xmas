open QCheck2.Gen
open Xmas

let num_char = char_range '0' '9'
let alpha_char = char_range 'a' 'z'
let big_alpha_char = char_range 'A' 'Z'
let alphanumeric = oneof [ num_char; alpha_char; big_alpha_char ]
let num_string = string_size (1 -- 10) ~gen:num_char
let small_string = string_size (1 -- 10) ~gen:alphanumeric
