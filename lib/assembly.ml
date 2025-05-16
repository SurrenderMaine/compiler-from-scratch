open Parser

type operand = Imm of int | Register
type instruction = Mov of operand * operand | Ret
type function_definition = Function of identifier * instruction list
type program = Program of function_definition

let convert_expression = function
| AST_Constant c -> Imm c

let convert_statement = function
| AST_Return expr -> [Mov (convert_expression expr, Register) ; Ret]

let convert_function = function
| AST_Function (name, statement) -> Function (name, convert_statement statement)

let convert_program = function
| AST_Program f -> Program (convert_function f)

let emit_operand = function
| Register -> "%eax"
| Imm i -> "$" ^ string_of_int i

let emit_instruction oc = function
| Mov (src, dest) -> 
  begin
    let src_str = emit_operand src in
    let dest_str = emit_operand dest in
    output_string oc ("\n\tmovl " ^ src_str ^ ", " ^ dest_str)
  end
| Ret -> output_string oc "\n\tret"

let emit_function oc = function
| Function (AST_ID name, instructions) ->
  begin
    output_string oc ("\n\t.globl " ^ name);
    output_string oc ("\n" ^ name ^ ":");
    List.iter (fun i -> emit_instruction oc i) instructions
  end

let emit_program oc = function
| Program f -> 
  begin
    emit_function oc f;
    output_string oc "\n\t.section .note.GNU-stack,\"\",@progbits\n";
    close_out oc
  end

let emit output program = 
  let instructions = convert_program program in
  let oc = open_out output in
  emit_program oc instructions