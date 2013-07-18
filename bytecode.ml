open Printf
open Bytecode_types

let string_of_instruction = function
	  Lit(integer) -> sprintf "Lit %d" integer
	| Bin(operator) -> "Bin " ^ Ast.string_of_operator operator

let print_bytecode stmt =
	let instructions = Compile.translate stmt in
	List.iter (fun x -> print_endline (string_of_instruction x)) instructions;;
