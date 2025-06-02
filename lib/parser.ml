open Tokens

type unary_operator = AST_Complement | AST_Negate
type expression = AST_Constant of int | AST_Unary of unary_operator * expression
type statement = AST_Return of expression
type func_def = AST_Function of string * statement
type program = AST_Program of func_def

let print_unary_operator = function
| AST_Complement -> "~"
| AST_Negate -> "-"

let rec print_expression = function
| AST_Constant i -> "AST_Constant ( " ^ string_of_int i ^ " )"
| AST_Unary (op, expr) -> "AST_Unary ( " ^ print_unary_operator op ^ " " ^ print_expression expr

let print_statement = function
| AST_Return expr -> "AST_Return ( " ^ print_expression expr ^ " )"

let print_fun = function
| AST_Function (id, st) -> "Function ( name=\"" ^ id ^ "\", body=" ^ print_statement st ^ " )"

let print_ast = function
| AST_Program f -> Format.printf "AST_Program ( %s )\n" (print_fun f)

let parse_error str =
    Format.(fprintf err_formatter "Parsing error: %s\n") str;
    exit 1

let expect_error expected actual =
    let expected_str = token_to_string expected in
    let actual_str = token_to_string actual in
    Format.(fprintf err_formatter "Parsing error: Expected \"%s\" but found \"%s\"\n" expected_str actual_str);
    exit 1

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
    | (Complement, Complement :: xs)
    | (Negate, Negate :: xs)
    | (Decrement, Decrement :: xs)
    | (Semicolon,   Semicolon :: xs) -> xs
    | (t, actual :: _) -> expect_error t actual
    | (_, []) -> parse_error "Empty list passed to expect"

let rec parse_expression = function
| Constant c :: tokens -> (tokens, AST_Constant c)
| Open_Paren :: tokens -> 
    begin
        let (tokens', inner_expr) = parse_expression tokens in
        let tokens'' = expect Close_Paren tokens' in
        (tokens'', inner_expr)
    end
| Negate :: tokens ->
    begin
        let (tokens', expr) = parse_expression tokens in
        (tokens', AST_Unary (AST_Negate, expr))
    end
| Complement :: tokens -> 
    begin
        let (tokens', expr) = parse_expression tokens in
        (tokens', AST_Unary (AST_Complement, expr))
    end
| _ -> parse_error "Expression"

let parse_identifier = function
| ID s :: tokens -> (tokens, s)
| _ -> parse_error "Identifier"

let parse_statement tokens =
    let (tokens', return_val) = expect Return tokens |> parse_expression in
    let tokens'' = expect Semicolon tokens' in
    (tokens'', AST_Return return_val)

let parse_function tokens =
    let (tokens', id) = expect Int tokens |> parse_identifier in
    let (tokens'', s) = expect Open_Paren tokens' |>
                        expect Void |>
                        expect Close_Paren |>
                        expect Open_Brace |>
    parse_statement in
    (expect Close_Brace tokens'', AST_Function (id, s))

let parse tokens = 
    let (_, p) = parse_function tokens in
    AST_Program p