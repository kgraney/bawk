open Printf
open Bytecode_types
open Ast_types

let string_of_instruction = function
	  Lit(integer) -> sprintf "Lit %d" integer
	| Bin(operator) -> "Bin " ^ Ast.string_of_operator operator
	| Rdb(num) -> sprintf "Rdb %d" num
	| Jsr(num) -> sprintf "Jsr %d" num
	| Rts(num) -> sprintf "Rts %d" num
	| Lod(num) -> sprintf "Lod %d" num
	| Str(num) -> sprintf "Str %d" num
	| Bra(addr) -> sprintf "Bra %d" addr
	| Beq(addr) -> sprintf "Beq %d" addr
	| Bne(addr) -> sprintf "Bne %d" addr
	| Beo(addr) -> sprintf "Beo %d" addr
	| Drp -> "Drp"
	| Ldp -> "Ldp"
	| Skp -> "Skp"
	| Lfp -> "Lfp"
	| Sfp -> "Sfp"
	| Ent -> "Ent"
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
	let globals = Array.make 1024 0 in
	let rec exec fp sp pc =
		(* Printf.printf "(fp=%d sp=%d pc=%d) " fp sp pc;
				Array.iter (fun d -> Printf.printf "%d " d) stack;
		Printf.printf "\n"; *)
		match instructions.(pc) with
			  Lit i -> stack.(sp) <- i;
				exec fp (sp + 1) (pc + 1)
			| Bin op ->
				let op1 = stack.(sp - 2) and op2 = stack.(sp - 1) in
				stack.(sp - 2) <- (
					let boolean i = if i then 1 else 0 in
					match op with
						  Add -> op1 + op2
						| Subtract -> op1 - op2
						| Multiply -> op1 * op2
						| Divide -> op1 / op2
						| Equal -> boolean (op1 == op2)
						| Neq -> boolean (op1 != op2)
						| Less -> boolean (op1 < op2)
						| Leq -> boolean (op1 <= op2)
						| Greater -> boolean (op1 > op2)
						| Geq -> boolean (op1 >= op2)
				);
				exec fp (sp - 1) (pc + 1)
			| Jsr (-1) -> print_endline (string_of_int stack.(sp - 1));
				exec fp (sp - 1) (pc + 1)
			| Rdb (1) ->
				stack.(sp) <- Reader.read_byte on_file;
				(* Printf.printf "Read byte: %x\n" stack.(sp); *)
				exec fp (sp + 1) (pc + 1)
			| Rdb (n) ->
				stack.(sp) <- Reader.read_unsigned on_file n;
				exec fp (sp + 1) (pc + 1)
			| Ldp ->
				stack.(sp) <- Reader.get_pos on_file;
				(* Printf.printf "Store position: %d at %d\n" stack.(sp) sp; *)
				exec fp (sp + 1) (pc + 1)
			| Skp ->
				Reader.set_pos on_file stack.(sp - 1);
				(* Printf.printf "Set position: %d\n" stack.(sp - 1); *)
				exec fp (sp - 1) (pc + 1)
			| Bne addr -> if stack.(sp - 1) == 0 then
				exec fp (sp - 1) (pc + 1) else
				exec fp (sp - 1) addr
			| Bra addr -> exec fp sp addr
			| Beo addr ->
				if Reader.is_eof on_file then exec fp sp addr
				else exec fp sp (pc + 1)
			| Jsr addr ->
				stack.(sp) <- pc + 1;
				exec fp (sp + 1) addr
			| Rts(num) ->
				(* Printf.printf "\nReturn to %d from %d (fp=%d, num=%d, sp=%d)\n" stack.(fp - 1) (fp - 1) stack.(sp - 1) num sp; *)
				exec stack.(fp) (fp - num) stack.(fp - 1)
			| Lod(-2) -> (* RP variable *)
				stack.(sp) <- Reader.get_pos on_file;
				exec fp (sp + 1) (pc + 1)
			| Lod index ->
				(
					if (index > 0) then stack.(sp) <- globals.(index)
					else stack.(sp) <- stack.(fp + index + 100 - 2)
				);
				exec fp (sp + 1) (pc + 1)

			| Str index ->
				(
					if (index > 0) then globals.(index) <- stack.(sp - 1)
					else stack.(fp + index + 100 - 2) <- stack.(sp - 1)
				);
				exec fp (sp - 1) (pc + 1)
			| Ent ->
				stack.(sp) <- fp;
				exec sp (sp + 1) (pc + 1)
			| Lfp ->
				exec stack.(sp) (sp - 1) (pc + 1)
			| Drp ->
				exec fp (sp - 1) (pc + 1)
			| Hlt -> ()
	in exec 0 0 0

let execute_bytecode instructions on_file =
	execute_instructions (Array.of_list instructions) on_file;;
