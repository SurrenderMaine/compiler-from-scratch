(* open Str *)
open Tokens

let remove_token token str =
  Re.replace_string ~pos:0 token ~by:"" ~all:false str

let extract_token l =
  match l with 
  | _ when Re.execp ~pos:0 int_str l -> (int_str, Int)
  | _ when Re.execp ~pos:0 void_str l -> (void_str, Void)
  | _ when Re.execp ~pos:0 return_str l -> (return_str, Return)
  | _ when Re.execp ~pos:0 open_paren_str l -> (open_paren_str, Open_Paren)
  | _ when Re.execp ~pos:0 close_paren_str l -> (close_paren_str, Close_Paren)
  | _ when Re.execp ~pos:0 open_brace_str l -> (open_brace_str, Open_Brace)
  | _ when Re.execp ~pos:0 close_brace_str l -> (close_brace_str, Close_Brace)
  | _ when Re.execp ~pos:0 semicolon_str l -> (semicolon_str, Semicolon)
  | _ when Re.execp ~pos:0 complement_str l -> (complement_str, Complement)
  | _ when Re.execp ~pos:0 decrement_str l -> (decrement_str, Decrement)
  | _ when Re.execp ~pos:0 negate_str l -> (negate_str, Negate)
  | _ when Re.execp ~pos:0 constant_str l -> 
    begin
      match Re.exec_opt constant_str l with
      | Some g -> (constant_str, Constant (Re.Group.get g 0 |> int_of_string))
      | None -> exit 1
    end
  | _ when Re.execp ~pos:0 identifier_str l -> 
    begin
      match Re.exec_opt identifier_str l with
      | Some g -> (identifier_str, ID (Re.Group.get g 0))
      | None -> exit 1
    end
  | _ -> Format.printf "error\n"; exit 1

let rec process_line tokens l =
  let l_nw = String.trim l in
  if String.length l_nw > 0 then
    let (r, t) = extract_token l_nw in
    let l' = remove_token r l_nw in
    process_line (t :: tokens) l'
  else
    tokens

let rec tokenize tokens ic =
  try
    let line = input_line ic in
    let tokens' = line |> String.trim |> process_line tokens in
    tokenize tokens' ic
  with _ ->
    List.rev tokens

let lexer input =
  open_in input |> tokenize []