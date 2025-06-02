open OUnit2
open Lib

let test_dir = (Sys.getcwd ()) ^ Filename.dir_sep ^ ".." ^ Filename.dir_sep ^ ".." ^ Filename.dir_sep ^ ".." ^ Filename.dir_sep ^ "code-samples"

let return_2 = Filename.concat test_dir "return_2.c"
let return_2_op = Filename.concat test_dir "return_2_op.c"

let return_2_ast = let open Parser in (AST_Program (AST_Function ("main", AST_Return ( AST_Constant 2 ) ) ) )
let return_2_op_ast = let open Parser in (AST_Program (AST_Function ("main", AST_Return ( AST_Unary ( AST_Complement, ( AST_Unary ( AST_Negate, AST_Constant 2 ) ) ) ) ) ) )

let test_parser_tree expected file =
  Lexer.lexer file |> Parser.parse |> assert_equal expected

let testing_files =
  "Testing the parser on files" >::: [
    "Parser: return_2" >:: (fun _ -> test_parser_tree return_2_ast return_2);
    "Parser: return_2_op" >:: (fun _ -> test_parser_tree return_2_op_ast return_2_op)
  ]

let all_tests : test list =
  [
    testing_files
  ]

let () = run_test_tt_main (TestList all_tests)