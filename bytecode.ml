open Printf
open Bytecode_types
open Ast_types

let string_of_instruction = function
	  Lit(integer) -> sprintf "Lit %d" integer
	| Bin(operator) -> "Bin " ^ Ast.string_of_operator operator
	| Rdb(num) -> sprintf "Rdb %d" num
	| Jsr(num) -> sprintf "Jsr %d" num
	| Bra(addr) -> sprintf "Bra %d" addr
	| Beq(addr) -> sprintf "Beq %d" addr
	| Bne(addr) -> sprintf "Bne %d" addr
	| Ldp -> "Ldp"
	| Skp -> "Skp"
	| Hlt -> "Hlt"

	| Label(id) -> sprintf "Label %d (PSEUDO)" id

let is_pseudo = function
	  Label(id) -> true
	| _ -> false

let enumerate_instructions lst =
	Utile.enumerate ~step:(fun x y ->
		if is_pseudo x then y
		else y + 1) lst;;

let print_bytecode instructions =
	List.iter (fun x -> let (i, ins) = x in
			printf "%9d: %s\n" i (string_of_instruction ins))
		(enumerate_instructions instructions);;

let execute_instructions instructions on_file =
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
				exec fp (sp - 1) (pc + 1)
			| Rdb (1) ->
				stack.(sp) <- Reader.read_byte on_file;
				(* Printf.printf "Read byte: %x\n" stack.(sp); *)
				exec fp (sp + 1) (pc + 1)
			| Ldp ->
				stack.(sp) <- Reader.get_pos on_file;
				(* Printf.printf "Store position: %d\n" stack.(sp); *)
				exec fp (sp + 1) (pc + 1)
			| Skp ->
				Reader.set_pos on_file stack.(sp - 1);
				(* Printf.printf "Set position: %d\n" stack.(sp - 1); *)
				exec fp (sp - 1) (pc + 1)
			| Bne addr -> if stack.(sp - 1) == 0 then
				exec fp sp (pc + 1) else
				exec fp (sp - 1) addr
			| Bra addr -> exec fp sp addr
			| Hlt -> ()
	in exec 0 0 0

let execute_bytecode instructions on_file =
	execute_instructions (Array.of_list instructions) on_file;;
