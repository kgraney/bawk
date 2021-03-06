(** The main function of the bawk compiler *)
open Compile

module StringMap = Map.Make(String) (* TODO: can we use the other def? *)

(** Possible actions for the compiler to take *)
type action = Ast | Compile | Execute
	| D_print_clean_env

(** [decode_action] reads the [argv] array of command line arguments and returns
	the action that the compiler is being asked to take.  This is fairly
	primitive, and only the first argument is used to make the decision. *)
let decode_action argv =
	if Array.length argv > 1 then
		List.assoc argv.(1) [
			("-ast", Ast);
			("-c", Compile);
			("-e", Execute);
			("-D-print-clean-env", D_print_clean_env)
		]
	else Execute;;

let decode_in_file argv =
	if Array.length argv > 2 then
		open_in_bin argv.(2)
	else
		open_in_bin "/dev/random";;


(** [parse_channel] runs an input channel through the lexer and parser phases
	of the compiler returning an AST. *)
let parse_channel channel =
	let lexbuf = Lexing.from_channel channel in
	let program = Parser.program Scanner.token lexbuf in
	program;;

let _ =
	let action = decode_action Sys.argv in
	let in_file = decode_in_file Sys.argv in
	match action with
		  Ast -> Ast.print_tree (parse_channel stdin)
		| Compile -> Bytecode.print_bytecode
			(Compile.translate_program (parse_channel stdin))
		| Execute -> Bytecode.execute_bytecode
			(Compile.translate_program (parse_channel stdin)) in_file

		(* Debug actions *)
		| D_print_clean_env ->
			let env = Compile.clean_environment in
			Printf.printf "Starting symbol table:\n";
			StringMap.iter (fun s i -> Printf.printf "\t%d %s\n" i s)
					env.symbol_map;;