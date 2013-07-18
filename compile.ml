open Ast_types
open Bytecode_types

let translate_expr expr =
	match expr with
	  LitInt(integer) ->
		[Bytecode_types.Lit(10)]

let rec translate stmt = 
	match stmt with
	  Block(stmt_list) ->
		List.flatten (List.map translate stmt_list)
	| Expr(expr) ->
		translate_expr expr
	| Pattern(pat_expr, stmt) ->
		[];;
