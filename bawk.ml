(** The main function of the bawk compiler *)

(** Possible actions for the compiler to take *)
type action = Ast | Compile

(** [decode_action] reads the [argv] array of command line arguments and returns
	the action that the compiler is being asked to take.  This is fairly
	primitive, and only the first argument is used to make the decision. *)
let decode_action argv =
	if Array.length argv > 1 then
		List.assoc argv.(1) [
			("-ast", Ast);
			("-c", Compile);
		]
	else Compile;;

(** [parse_channel] runs an input channel through the lexer and parser phases
	of the compiler returning an AST. *)
let parse_channel channel =
	let lexbuf = Lexing.from_channel channel in
	let program = Parser.program Scanner.token lexbuf in
	program;;

let _ =
	let action = decode_action Sys.argv in
	match action with
		  Ast -> Ast.print_tree (parse_channel stdin)
		| Compile -> Bytecode.print_bytecode (parse_channel stdin)
