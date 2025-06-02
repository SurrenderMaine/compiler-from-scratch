open Lib

let compiler input output =
  if Array.exists (fun arg -> arg = "--lex") Sys.argv then
    let _ =
      Lexer.lexer input 
    in
    ()
  else if Array.exists (fun arg -> arg = "--parse") Sys.argv then
    let _ = 
      Lexer.lexer input |>
      Parser.parse 
    in
    ()
  else if Array.exists (fun arg -> arg = "--ir") Sys.argv then
    let _ = 
      Lexer.lexer input |>
      Parser.parse |>
      Ir.emit_program
    in
    ()
  else 
    Lexer.lexer input |>
    Parser.parse |>
    Ir.emit_program |>
    Assembly.convert_program |>
    Assembly.emit output

let compiler_driver input =
  let dir = Filename.dirname input in
  let file = Filename.basename input |> Filename.remove_extension in
  let pp_file = Filename.concat dir file ^ ".i" in
  let s_file = Filename.concat dir file ^ ".s" in
  let executable = Filename.concat dir file in
  let _ = Sys.command ("gcc -E -P " ^ input ^ " -o " ^ pp_file) in
  let _ = compiler pp_file s_file; Sys.remove pp_file in
  let _ = Sys.command ("gcc " ^ s_file ^ " -o " ^ executable) in
  ()

let is_valid_input =
  let argc = Array.length Sys.argv in
  if (argc >= 2) then
    begin
      let input = Array.get Sys.argv 1 in
      let is_c_file = ((Filename.extension input) = ".c") in
      let is_valid_file = Sys.file_exists input in
      is_c_file && is_valid_file
    end
  else
    false 

let () = 
  if is_valid_input then begin
    Array.get Sys.argv 1 |> compiler_driver; exit 0
  end else
    exit 1