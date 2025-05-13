open Tokens

type identifier = Ident of string
type expression = Const of int 
type statement = Ret of expression
type func_def = Fun of identifier * statement
type program = Program of func_def

let print_expression = function
| Const i -> "Constant ( " ^ string_of_int i ^ " )"

let print_statement = function
| Ret expr -> "Return( " ^ print_expression expr ^ " )"

let print_fun = function
| Fun (Ident id, st) -> "Function( name=\"" ^ id ^ "\", body=" ^ print_statement st ^ " )"

let print_ast = function
| Program f -> Format.printf "Program( %s )\n" (print_fun f)

let expect_error expected actual =
    let expected_str = token_to_string expected in
    let actual_str = token_to_string actual in
    Format.fprintf Format.err_formatter "Parsing error: Expected \"%s\" but found \"%s\"\n" expected_str actual_str 

let expect expected tokens =
    match (expected, tokens) with
    | (Constant _,  (Constant _) :: xs)
    | (ID _,        (ID _) :: xs)
    | (Int,         Int :: xs)
    | (Void,        Void :: xs)
    | (Return,      Return :: xs)
    | (Open_Paren,  Open_Paren :: xs) 
    | (Close_Paren, Close_Paren :: xs)
    | (Open_Brace,  Open_Brace :: xs)
    | (Close_Brace, Close_Brace :: xs)
    | (Semicolon,   Semicolon :: xs) -> xs
    | (t, actual :: _) -> expect_error t actual; exit 1
    | _ -> Format.fprintf Format.err_formatter "Parsing error: Empty list passed to expect\n"; exit 1

let parse_expression = function
| Constant c :: xs -> (xs, Const c)
| _ -> Format.printf "Parsing error: Expression\n"; exit 1

let parse_identifier = function
| ID s :: xs -> (xs, Ident s)
| _ -> Format.printf "Parsing error: Identifier\n"; exit 1

let parse_statement tokens =
    let (tokens', return_val) = expect Return tokens |> parse_expression in
    let tokens'' = expect Semicolon tokens' in
    (tokens'', Ret return_val)

let parse_function tokens =
    let (tokens', id) = expect Int tokens |> parse_identifier in
    let (tokens'', s) = expect Open_Paren tokens' |>
                        expect Void |>
                        expect Close_Paren |>
                        expect Open_Brace |>
    parse_statement in
    (expect Close_Brace tokens'', Fun (id, s))

let parse tokens = 
    let (_, p) = parse_function tokens in
    print_ast (Program p)