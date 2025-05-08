open Lib

let compiler input =
  Lexer.lexer input;
  (* Parser *)
  (* Codegen *)
  ()

let compiler_driver filename =
  let _ = Sys.command ("gcc -E -P " ^ filename ^ "test.i") in
  let _ = compiler "test.i" in
  let _ = Sys.command ("gcc test.s -o test") in
  ()

let is_valid_input =
  let argc = Array.length Sys.argv in
  if (argc = 2) then
    begin
      let filename = Array.get Sys.argv 1 in
      let is_c_file = String.ends_with ~suffix:".c" filename in
      let is_valid_file = Sys.file_exists filename in
      is_c_file && is_valid_file
    end
  else
    false

let () = 
  if is_valid_input then
    begin
      let filename = Array.get Sys.argv 1 in
      compiler_driver filename
    end
  else
    exit 1