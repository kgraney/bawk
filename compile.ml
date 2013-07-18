open Ast_types
open Bytecode_types

let rec translate_expr expr =
	match expr with
	  LitInt(integer) ->
		[Bytecode_types.Lit(integer)]
	| Binopt(e1, op, e2) ->
		translate_expr e1 @ translate_expr e2 @ [Bin op]

let rec translate stmt = 
	match stmt with
	  Block(stmt_list) ->
		List.flatten (List.map translate stmt_list)
	| Expr(expr) ->
		translate_expr expr
	| Pattern(pat_expr, stmt) ->
		[];;
