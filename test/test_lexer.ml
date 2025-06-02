open OUnit2
open Lib
open Tokens

let test_dir = (Sys.getcwd ()) ^ Filename.dir_sep ^ ".." ^ Filename.dir_sep ^ ".." ^ Filename.dir_sep ^ ".." ^ Filename.dir_sep ^ "code-samples"

let return_2 = Filename.concat test_dir "return_2.c"
let return_2_op = Filename.concat test_dir "return_2_op.c"

let return_2_tokens = let open Tokens in [Int; ID "main"; Open_Paren; Void; Close_Paren; Open_Brace; Return; Constant 2; Semicolon; Close_Brace]
let return_2_op_tokens = let open Tokens in [Int; ID "main"; Open_Paren; Void; Close_Paren; Open_Brace; Return; Complement; Open_Paren; Negate; Constant 2; Close_Paren; Semicolon; Close_Brace]

let test_lexer_tokens expected file =
  Lexer.lexer file |> assert_equal expected 

let extract_token str = 
  let (_, token) = Lexer.extract_token str in
  token

let testing_tokens = 
  "Testing extract_tokens" >::: [
    "ID \"main\"" >:: (fun _ -> assert_equal (ID "main") (extract_token "main"));
    "Constant 4335" >:: (fun _ -> assert_equal (Constant 4335) (extract_token "4335"));
    "Int" >:: (fun _ -> assert_equal (Int) (extract_token "int"));
    "Void" >:: (fun _ -> assert_equal (Void) (extract_token "void"));
    "Return" >:: (fun _ -> assert_equal (Return) (extract_token "return"));
    "Open_Paren" >:: (fun _ -> assert_equal (Open_Paren ) (extract_token "("));
    "Close_Paren" >:: (fun _ -> assert_equal (Close_Paren ) (extract_token ")"));
    "Open_Brace" >:: (fun _ -> assert_equal (Open_Brace ) (extract_token "{"));
    "Close_Brace" >:: (fun _ -> assert_equal (Close_Brace ) (extract_token "}"));
    "Semicolon" >:: (fun _ -> assert_equal (Semicolon) (extract_token ";"));
    "Complement" >:: (fun _ -> assert_equal (Complement) (extract_token "~"));
    "Negate" >:: (fun _ -> assert_equal (Negate) (extract_token "-"));
    "Decrement" >:: (fun _ -> assert_equal (Decrement) (extract_token "--"));
  ]

let testing_files =
  "Testing the lexer on files" >::: [
    "return_2" >:: (fun _ -> test_lexer_tokens return_2_tokens return_2);
    "return_2_op" >:: (fun _ -> test_lexer_tokens return_2_op_tokens return_2_op)
  ]

let all_tests : test list =
  [
    testing_tokens;
    testing_files
  ]

let () = run_test_tt_main (TestList all_tests)