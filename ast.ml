open Ast_types
open Printf

let make_link n1 n2 =
	printf "%d -> %d\n" n1 n2;;

let conditional_link_with_parent this_id parent_id =
	if parent_id != -1 then make_link parent_id this_id
	else ();;

let make_terminal ~this:this_id ?parent:(parent_id=(-1)) label =
	printf "%d [label=\"%s\" shape=hexagon style=filled]\n" this_id label;
	conditional_link_with_parent this_id parent_id;;

let make_nonterminal ~this:this_id ?parent:(parent_id=(-1)) label =
	printf "%d [label=\"%s\" shape=plaintext]\n" this_id label;
	conditional_link_with_parent this_id parent_id;;

let folded_printer func parent_id =
	(fun id x -> let this_id = id + 1 in
		func x this_id parent_id );;

let print_plain str id parent =
	make_nonterminal str ~this:id ~parent:parent;
	id + 1

let size_of_bind_type = function
	| Int_1_byte -> 1
	| Int_2_bytes -> 2
	| Int_4_bytes -> 4
	| UInt_1_byte -> 1
	| UInt_2_bytes -> 2
	| UInt_4_bytes -> 4

let string_of_bind_type = function
	| Int_1_byte -> "int1"
	| Int_2_bytes -> "int2"
	| Int_4_bytes -> "int4"
	| UInt_1_byte -> "uint1"
	| UInt_2_bytes -> "uint2"
	| UInt_4_bytes -> "uint4"

let is_signed_type = function
	| Int_1_byte -> true
	| Int_2_bytes -> true
	| Int_4_bytes -> true
	| UInt_1_byte -> false
	| UInt_2_bytes -> false
	| UInt_4_bytes -> false

let string_of_operator = function
	| Add -> "Add"
	| Subtract -> "Sub"
	| Multiply -> "Mul"
	| Divide -> "Div"
	| Equal -> "Eq"
	| Neq -> "Neq"
	| Less -> "Lt"
	| Leq -> "Leq"
	| Greater -> "Gt"
	| Geq -> "Geq"

(* each *_print function returns the next id available for use *)
let print_tree prog =
	let rec stmt_print root id parent =
		match root with
		| Pattern(pat_expr, stmt) ->
			let pattern_id = id + 1 in
			let stmt_id = pat_expr_print pat_expr pattern_id id in
			let consumed_ids = stmt_print stmt (stmt_id + 1) stmt_id in
			make_nonterminal "pattern" ~this:id ~parent:parent;
			make_nonterminal "statement" ~this:stmt_id ~parent:id;
			consumed_ids

		| Block(statement_lst) ->
			make_nonterminal "block" ~this:id ~parent:parent;
			List.fold_left (folded_printer stmt_print id) id statement_lst

		| Expr(expr) ->
			make_nonterminal "expression" ~this:id ~parent:parent;
			expr_print expr (id + 1) id

		| FunctionDecl(decl) ->
			let stmt_id = stmt_print decl.body (id + 1) id in
			let consumed_ids = List.fold_left
					(folded_printer print_plain stmt_id) (stmt_id + 1)
					decl.arguments
			in
			make_nonterminal "arguments" ~this:stmt_id ~parent:id;
			make_nonterminal (sprintf "fdef:%s" decl.fname) ~this:id
					~parent:parent;
			consumed_ids
		| If(cond, stmt1, stmt2) ->
			let if_id = expr_print cond (id + 1) id in
			let ifstmt = stmt_print stmt1 if_id id in
			let consumed_ids = stmt_print stmt2 ifstmt id in
			make_nonterminal "if" ~this:id ~parent:parent;
			consumed_ids

		(* | _ ->
			make_nonterminal "other_stmt" ~this:id ~parent:parent;
			id + 1 *)

	and expr_print expr id parent =
		match expr with
		| LitInt(value) ->
			make_terminal (string_of_int value) ~this:id ~parent:parent;
			id + 1
		| Binopt(e1, op, e2) ->
			let e2_id = expr_print e1 (id + 1) id in
			let consumed_ids = expr_print e2 e2_id id in
			make_nonterminal (string_of_operator op) ~this:id ~parent:parent;
			consumed_ids
		| Call(name, arg_list) ->
			let name_id = id + 1 in
			let arg_id = name_id + 1 in
			make_nonterminal "call" ~this:id ~parent:parent;
			make_terminal name ~this:name_id ~parent:id;
			make_nonterminal "arguments" ~this:arg_id ~parent:id;
			List.fold_left (folded_printer expr_print arg_id) arg_id arg_list
		| LitString(str) ->
			make_terminal str ~this:id ~parent:parent;
			id + 1;
		| Assign(name, expr) ->
			make_nonterminal "assign" ~this:id ~parent:parent;
			make_nonterminal name ~this:(id + 1) ~parent:id;
			expr_print expr (id + 2) id
		| _ ->
			make_nonterminal "other_expr" ~this:id ~parent:parent;
			id + 1

	and pat_expr_print lst id parent =
		make_nonterminal "pat_expr" ~this:id ~parent:parent;
		List.fold_left (folded_printer pat_token_print id) id lst

	and pat_token_print token id parent =
		match token with
		| Binding(literal, bind_type) ->
			let literal_id = id + 1 in
			let type_id = id + 2 in
			make_nonterminal "binding" ~this:id ~parent:parent;
			make_terminal literal ~this:literal_id ~parent:id;
			make_terminal (string_of_bind_type bind_type)
					~this:type_id ~parent:id;
			id + 3
		| Literal(literal) ->
			make_nonterminal "literal" ~this:id ~parent:parent;
			make_terminal literal ~this:(id + 1) ~parent:id;
			id + 2
		| PatternBytes(values) ->
			make_nonterminal "bytes" ~this:id ~parent:parent;
			List.fold_left (folded_printer pat_token_print id) id values
		| PatternByte(value) ->
			make_terminal (string_of_int value) ~this:id ~parent:parent;
			id + 1
		| PatString(str) ->
			make_terminal (sprintf "'%s'" str) ~this:id ~parent:parent;
			id + 1
		(*| _ ->
			make_nonterminal "foo" ~this:(id+1) ~parent:id;
			id + 2*)
	in
	print_string "digraph AST {\n";
	print_string "ordering = out;\n";
	ignore (stmt_print prog 0 0);
	print_string "}";;
