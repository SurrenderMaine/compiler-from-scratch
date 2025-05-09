let identifier = Str.regexp "[a-zA-Z_]\\w*\\b"
let constant = Str.regexp "[0-9]+\\b"
let int_keyword = Str.regexp "int\\b"
let void_keyword =  Str.regexp "void\\b"
let return_keyword = Str.regexp "return\\b"
let open_paren = Str.regexp_string "("
let close_paren = Str.regexp_string ")"
let open_brace = Str.regexp "{"
let close_brace = Str.regexp "}"
let semicolon = Str.regexp ";"

type token = ID | Constant | Int | Void | Return | Open_Paren | Close_Paren | Open_Brace | Close_Brace | Semicolon

let extract_first_token line =
  if 

let rec tokenize ic =
  try
    let line = input_line ic in
    let no_whitespace = line |> String.trim in
    let 
    tokenize ic
  with _ ->
    ()

let lexer input =
  open_in input |> tokenize

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