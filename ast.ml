open Ast_types

let print_tree prog =
	let rec rprint = function
		| [] -> ()
		| h::t -> print_string "items\n"; rprint t
	in rprint prog;;
