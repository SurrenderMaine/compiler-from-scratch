let identifier = Str.regexp "[a-zA-Z_]\\w*\\b"
let constant = Str.regexp "[0-9]+\\b"
let int_keyword = Str.regexp "int\\b"
let void_keyword =  Str.regexp "void\\b"
let return_keyword = Str.regexp "return\\b"
let open_paren = Str.regexp "\\("
let close_paren = Str.regexp "\\)"
let open_brace = Str.regexp "{"
let close_brace = Str.regexp "}"
let semicolon = Str.regexp ";"

let lexer _ =
  Format.printf "Lexing\n"

(* let tokenize input = 
   while input isnt empty:
    if input starts with whitespace:
      trim whitespace from start of input
    else:
      find longest match at start of input for any regex in Table 1-1
      if no match is found, raise an error
      convert matching substring into a token
      remove matching substring from start of input
  
*)