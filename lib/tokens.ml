(* open Str *)

type token = 
| ID of string 
| Constant of int 
| Int 
| Void 
| Return 
| Open_Paren 
| Close_Paren 
| Open_Brace 
| Close_Brace 
| Semicolon
| Complement
| Negate
| Decrement

let identifier_str      = Re.Pcre.regexp {|^[a-zA-Z_]\w*\b|} 
let constant_str        = Re.Pcre.regexp "^[0-9]+\\b" 
let int_str             = Re.Pcre.regexp "^int\\b"
let void_str            = Re.Pcre.regexp "^void\\b"
let return_str          = Re.Pcre.regexp "^return\\b"
let open_paren_str      = Re.Pcre.regexp "^\\("
let close_paren_str     = Re.Pcre.regexp "^\\)"
let open_brace_str      = Re.Pcre.regexp "^\\{"
let close_brace_str     = Re.Pcre.regexp "^\\}"
let semicolon_str       = Re.Pcre.regexp "^;"
let complement_str      = Re.Pcre.regexp "^~"
let negate_str          = Re.Pcre.regexp "^-"
let decrement_str       = Re.Pcre.regexp "^--"


let token_to_string = function
| ID s -> "ID " ^ s
| Constant c -> string_of_int c
| Int -> "int"
| Void -> "void"
| Return -> "return"
| Open_Paren -> "("
| Close_Paren -> ")"
| Open_Brace -> "{"
| Close_Brace -> "}"
| Semicolon -> ";"
| Complement -> "~"
| Negate -> "-"
| Decrement -> "--"