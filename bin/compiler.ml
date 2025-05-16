open Lib

let compiler input output =
  Lexer.lexer input |>
  Parser.parse |>
  Assembly.emit output

let compiler_driver input =
  let dir = Filename.dirname input in
  let file = Filename.basename input |> Filename.remove_extension in
  let pp_file = Filename.concat dir file ^ ".i" in
  let s_file = Filename.concat dir file ^ ".s" in
  let executable = Filename.concat dir file in
  let _ = Sys.command ("gcc -E -P " ^ input ^ " -o " ^ pp_file) in
  let _ = compiler pp_file s_file in
  let _ = Sys.command ("gcc " ^ s_file ^ " -o " ^ executable) in
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