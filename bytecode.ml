open Printf
open Bytecode_types
open Ast_types

let string_of_instruction = function
	  Lit(integer) -> sprintf "Lit %d" integer
	| Bin(operator) -> "Bin " ^ Ast.string_of_operator operator
	| Rdb(num_bytes) -> sprintf "Rdb %d" num_bytes
	| Jsr(num) -> sprintf "Jsr %d" num

let print_bytecode stmt =
	let instructions = Compile.translate_program stmt in
	List.iter (fun x -> print_endline (string_of_instruction x)) instructions;;

let execute_instructions instructions =
	let stack = Array.make 1024 0 in
	let rec exec fp sp pc =
		match instructions.(pc) with
			  Lit i -> stack.(sp) <- i;
				exec fp (sp + 1) (pc + 1)
			| Bin op ->
				let op1 = stack.(sp - 2) and op2 = stack.(sp - 1) in
				stack.(sp - 2) <- (
					match op with
						  Add -> op1 + op2
						| Subtract -> op1 - op2
						| Multiply -> op1 * op2
						| Divide -> op1 / op2
				);
				exec fp (sp - 1) (pc + 1)
			| Jsr (-1) -> print_endline (string_of_int stack.(sp - 1));
				exec fp sp (pc + 1)
	in exec 0 0 0

let execute_bytecode stmt =
	let instructions = Compile.translate_program stmt in
	execute_instructions (Array.of_list instructions);;
