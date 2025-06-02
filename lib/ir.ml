open Parser

type unary_operator = Complement | Negate
type value = Constant of int | Var of string 
type instruction = Return of value | Unary of unary_operator * value * value
type function_definition = Function of string * instruction list
type program = Program of function_definition

let temp = ref 0

let make_temporary () = 
  let num = !temp in
  temp := num + 1;
  "tmp." ^ string_of_int num

let convert_unop = function
| AST_Complement -> Complement
| AST_Negate -> Negate 

let rec emit_tacky expr instructions = 
  match expr with
  | AST_Constant c -> (instructions, Constant c)
  | AST_Unary (op, inner) -> 
    begin
      let (instructions', src) = emit_tacky inner instructions in
      let dst_name = make_temporary () in
      let dst = Var dst_name in
      let tacky_op = convert_unop op in
      let instructions'' = instructions' @ [Unary (tacky_op, src, dst)] in
      (instructions'', dst)
    end

(* let trav_unary = function
| Complement -> "Complement"
| Negate -> "Negate"

let trav_value = function
| Constant c -> string_of_int c
| Var s -> s

let trav_return = function
| Return v -> "Return " ^ trav_value v
| Unary (op, v1, v2) -> trav_unary op ^ " " ^ trav_value v1 ^ " " ^ trav_value v2

let trav_function_def = function
| Function (id, instructions) -> Format.printf "%s:\n" id; List.iter (fun instruction -> Format.printf "%s\n" (trav_return instruction)) instructions

let trav_program (Program f) = trav_function_def f *)

let emit_statement (AST_Return s) =
  let (instructions, dst) = emit_tacky s [] in
  instructions @ [Return dst]

let emit_function (AST_Function (name, s)) = 
  let instructions = emit_statement s in
  Function (name, instructions) 

let emit_program (AST_Program p) = 
  Program (emit_function p)