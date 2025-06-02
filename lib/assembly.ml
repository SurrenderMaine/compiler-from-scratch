open Ir

module StackMap = Map.Make(String)

type reg = AX | R10
type operand = Imm of int | Reg of reg | Pseudo of string | Stack of int
type unary_operator = Neg | Not
type instruction = 
| Mov of operand * operand 
| Unary of unary_operator * operand
| AllocateStack of int
| Ret
type function_definition = Function of string * instruction list
type program = Program of function_definition

(*************************************************************)
(* Functions for converting the ir ast to the assembly ast *)

let convert_operand = function
| Constant c -> Imm c
| Var n -> Pseudo n

let convert_operator = function
| Complement -> Not
| Negate -> Neg

let convert_instruction = function
| Return v -> [Mov (convert_operand v, Reg AX); Ret]
| Unary (o, s, d) -> 
  begin
    let op = convert_operator o in
    let src = convert_operand s in
    let dst = convert_operand d in
    [Mov (src, dst); Unary (op, dst)]
  end

let convert_function (Ir.Function (name, statement)) =
  let instructions = List.fold_right (fun i acc -> (convert_instruction i) @ acc) statement [] in
  Function (name, instructions)

let replace_pseudo stack offset = function
| Pseudo s -> 
  begin
    match StackMap.find_opt s stack with
    | Some v -> (Stack v, stack, offset)
    | None -> (Stack offset, StackMap.add s offset stack, offset - 4)
  end
| i -> (i, stack, offset)

let rec place_stack_operands stack offset instructions = function
| [] -> (instructions, stack)
| Mov (src, dst) :: xs -> 
  begin
    let (src', stack', offset') = replace_pseudo stack offset src in
    let (dst', stack'', offset'') = replace_pseudo stack' offset' dst in
    let instruction = Mov (src', dst') in
    place_stack_operands stack'' offset'' (instructions @ [instruction]) xs
  end 
| Unary (op, src) :: xs -> 
  begin
    let (src', stack', offset') = replace_pseudo stack offset src in
    let instruction = Unary (op, src') in
    place_stack_operands stack' offset' (instructions @ [instruction]) xs
  end 
| i :: xs -> place_stack_operands stack offset (instructions @ [i]) xs

let insert_alloc_stack stack instructions =
  let size = (StackMap.cardinal stack) * 4 in
  AllocateStack size :: instructions

let fix_invalid_mov instruction acc =
  match instruction with
  | Mov (Stack src, Stack dst) -> Mov (Stack src, Reg R10) :: Mov (Reg R10, Stack dst) :: acc
  | i -> (i :: acc)

let replace_instructions = function
| Program (Function (id, instructions)) ->
  begin
    let (instructions', stack) = place_stack_operands StackMap.empty (-4) [] instructions in
    let instructions'' = insert_alloc_stack stack instructions' in
    let instructions''' = List.fold_right fix_invalid_mov instructions'' [] in
    Program (Function (id, instructions'''))
  end 

(* let string_of_reg = function
| AX -> "AX"
| R10 -> "R10"

let string_of_operand = function
| Imm i -> "Imm " ^ string_of_int i
| Reg r -> "Reg " ^ string_of_reg r
| Pseudo p -> "Pseudo " ^ p
| Stack i -> "Stack " ^ string_of_int i

let string_of_operator = function
| Neg -> "Neg"
| Not -> "Not"

let string_of_instruction = function
| Mov (op1, op2) -> "Mov " ^ string_of_operand op1 ^ ", " ^ string_of_operand op2
| Unary (op1, op2) -> "Unary " ^ string_of_operator op1 ^ " " ^ string_of_operand op2
| AllocateStack s -> "AllocateStack " ^ string_of_int s
| Ret -> "Ret"

let print_program (Program (Function (_, instructions))) =
  List.iter (fun i -> Format.printf "%s\n" (string_of_instruction i)) instructions  *)

let convert_program (Ir.Program f) = 
  Program (convert_function f) |> replace_instructions

(*************************************************************)
(* Functions for printing the assembly ast to an output file *)

let operand_to_string = function
| Reg AX -> "%eax"
| Reg R10 -> "%r10d"
| Stack i -> string_of_int i ^ "(%rbp)"
| Imm i -> "$" ^ string_of_int i
| Pseudo _ -> exit 1

let print_label oc label =
  output_string oc ("\n" ^ label ^ ":")

let print_instruction oc instruction = 
  output_string oc ("\n\t" ^ instruction)

let operator_to_string = function
| Neg -> "negl"
| Not -> "notl" 

let emit_instruction oc = function
| Mov (src, dest) -> 
  begin
    let src_str = operand_to_string src in
    let dest_str = operand_to_string dest in
    print_instruction oc ("movl " ^ src_str ^ ", " ^ dest_str)
  end
| Ret -> 
  begin
    print_instruction oc "movq %rbp, %rsp";
    print_instruction oc "popq %rbp";
    print_instruction oc "ret"
  end
| Unary (op, operand) -> print_instruction oc (operator_to_string op ^ " " ^ operand_to_string operand) 
| AllocateStack i -> print_instruction oc ("subq $" ^ string_of_int i ^ ", %rsp")

let emit_function oc = function
| Function (name, instructions) ->
  begin
    print_instruction oc (".globl " ^ name);
    print_label oc name;
    print_instruction oc "pushq %rbp";
    print_instruction oc "movq %rsp, %rbp";
    List.iter (fun i -> emit_instruction oc i) instructions
  end

let emit_program oc = function
| Program f -> 
  begin
    emit_function oc f;
    print_instruction oc ".section .note.GNU-stack,\"\",@progbits\n";
    close_out oc
  end

let emit output program = 
  let oc = open_out output in
  let _ = emit_program oc program in
  close_out oc

(*************************************************************)