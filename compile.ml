open Ast_types
open Bytecode_types

module StringMap = Map.Make(String)

type env = {
	function_map: int StringMap.t; (* address for each defined function name *)
}

let built_in_functions = ["print"];;

let clean_environment =
	let built_ins =
		List.fold_left (fun map item ->
			let (value, key) = item in
			StringMap.add key (value - 1) map
		) StringMap.empty (Utile.enumerate ~step:(fun x -> x - 1)
				built_in_functions) in
	{
		function_map = built_ins;
	}

let rec translate_expr env expr =
	let recurse = translate_expr env in
	match expr with
	  LitInt(integer) ->
		[Bytecode_types.Lit(integer)]
	| Binopt(e1, op, e2) ->
		recurse e1 @ recurse e2 @ [Bin op]
	| Call(func_name, args) ->
		let function_addr = StringMap.find func_name env.function_map in
		(List.concat (List.map recurse args)) @ [Jsr function_addr]

let translated_pattern expr =
	[Rdb(10)]

let rec translate env stmt =
	let recurse = translate env in
	match stmt with
	  Block(stmt_list) ->
		List.flatten (List.map recurse stmt_list)
	| Expr(expr) ->
		translate_expr env expr
	| Pattern(pat_expr, stmt) ->
		translated_pattern pat_expr @
		recurse stmt;;

let translate_program stmt =
	let env = clean_environment in
	translate env stmt @ [Hlt];;
