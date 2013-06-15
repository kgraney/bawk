open Ast_types
open Printf

let make_link n1 n2 =
	printf "%d -> %d\n" n1 n2;;

let conditional_link_with_parent this_id parent_id =
	if parent_id != -1 then make_link parent_id this_id
	else ();;

let make_terminal ~this:this_id ?parent:(parent_id=(-1)) label =
	printf "%d [label=\"%s\" shape=\"hexagon\" style=\"filled\"]\n" this_id label;
	conditional_link_with_parent this_id parent_id;;

let make_nonterminal ~this:this_id ?parent:(parent_id=(-1)) label =
	printf "%d [label=\"%s\"]\n" this_id label;
	conditional_link_with_parent this_id parent_id;;

let folded_printer func parent_id =
	(fun id x -> let this_id = id + 1 in
		make_link parent_id this_id;
		func x this_id parent_id );;

(* each *_print function returns the next id available for use *)
let print_tree prog =
	let rec stmt_print root id parent =
		match root with
		| Pattern(pat_expr, stmt) ->
			let pattern_id = id + 1 in
			let stmt_id = pat_expr_print pat_expr pattern_id id in
			let consumed_ids = stmt_print stmt (stmt_id + 1) id in
			make_nonterminal "pattern" ~this:id;
			make_nonterminal "statement" ~this:stmt_id ~parent:id;
			make_link stmt_id (stmt_id + 1);
			consumed_ids

		| Block(statement_lst) ->
			make_nonterminal "block" ~this:id;
			List.fold_left (folded_printer stmt_print id) id statement_lst

		| Expr(expr) ->
			make_nonterminal "expression" ~this:id;
			expr_print expr (id + 1) id

		| _ -> printf "%d [label=\"other\"]\n" id; id;

	and expr_print expr id parent =
		match expr with
		| LitInt(value) ->
			make_terminal (string_of_int value) ~this:id ~parent:parent;
			id + 1
		| _ ->
			make_nonterminal "other_expr" ~this:id ~parent:parent;
			id + 1

	and pat_expr_print lst id parent =
		make_nonterminal "pat_expr" ~this:id ~parent:parent;
		List.fold_left (folded_printer pat_token_print id) id lst

	and pat_token_print token id parent =
		id + 1
	in
	print_string "digraph AST {\n";
	stmt_print prog 0 0;
	print_string "}";;
