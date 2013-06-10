open Ast_types
open Printf

let print_tree prog =
	let rec stmt_print root id parent =
		match root with
		| Pattern(pat_expr, stmt) ->
			let parent_id = id in
			let stmt_id = pat_print pat_expr (parent_id + 1) parent_id in
			let consumed_ids = stmt_print stmt (stmt_id + 1) parent_id in
			printf "%d [label=\"pat_expr\"]\n" (parent_id + 1);
			printf "%d -> %d;\n" parent_id (parent_id + 1);
			printf "%d [label=\"statement\"]\n" stmt_id;
			printf "%d -> %d;\n" parent_id stmt_id;
			printf "%d -> %d;\n" stmt_id (stmt_id + 1);
			printf "%d [label=\"pattern\"];\n" parent_id;
			consumed_ids

		| Block(statement_lst) ->
			let parent_id = id in
			let consumed_ids = List.fold_left
				(fun id x ->
					printf "%d -> %d\n" parent_id (id + 1);
					stmt_print x (id + 1) parent_id
				)
				parent_id statement_lst in
			printf "%d [label=\"block\"];\n" parent_id;
			consumed_ids

		| _ -> printf "%d [label=\"other\"]\n" id; id;
	and pat_print root id parent =
		id + 1
	in
	print_string "digraph AST {\n";
	stmt_print prog 0 0;
	print_string "}";;
