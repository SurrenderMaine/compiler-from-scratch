open Lib

let compiler input =
  Lexer.lexer input;
  (* Parser *)
  (* Codegen *)
  ()

let compiler_driver input =
  let dir = Filename.dirname input in
  let file = Filename.basename input |> Filename.remove_extension in
  let pp_file = Filename.concat dir file ^ ".i" in
  let _ = Sys.command ("gcc -E -P " ^ input ^ " -o " ^ pp_file) in
  let _ = compiler pp_file in
  (* let _ = Sys.command ("gcc test.s -o test") in *)
  ()

let is_valid_input =
  let argc = Array.length Sys.argv in
  if (argc = 2) then
    begin
      let input = Array.get Sys.argv 1 in
      let is_c_file = ((Filename.extension input) = ".c") in
      let is_valid_file = Sys.file_exists input in
      is_c_file && is_valid_file
    end
  else
    false

let () = 
  if is_valid_input then
    Array.get Sys.argv 1 |> compiler_driver
  else
    exit 1