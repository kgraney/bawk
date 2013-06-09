
(*
let rec eval = function
	| Ast.Block(x) -> eval_statements x
	| Ast.Expr(x) -> x

and eval_statements x =
	List.map eval x;;
*)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	Ast.print_tree program;;

(*  let result = eval program in
		print_endline (string_of_int result) *)
