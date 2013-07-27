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
		) StringMap.empty (Utile.enumerate ~step:(fun x y -> y - 1)
				built_in_functions) in
	{
		function_map = built_ins;
	}

let label_counter = ref 0;;
let get_new_label () =
	label_counter := !label_counter + 1;
	!label_counter;;

let form_label_map instructions =
	let arr = Array.make (!label_counter + 1) 0 in
	let enumerated = Bytecode.enumerate_instructions instructions in
	List.iter (fun x ->
		let (addr, instr) = x in
		match instr with
			  Label (id) -> arr.(id) <- addr; ()
			| _ -> ()
		) enumerated;
	arr;;

let resolve_labels instructions =
	let label_map = form_label_map instructions in
	let rec emit_resolution = function
		  [] -> []
		(* rewrite branches *)
		| Bne(id)::t -> Bne label_map.(id) :: emit_resolution t

		(* remove the pseudo instructions *)
		| Label(id)::t -> emit_resolution t

		(* all other instructions *)
		| h::t -> h :: emit_resolution t
	in emit_resolution instructions;;

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

let translated_pattern expr end_label =
	let rec check_item = function
		  PatternByte(value) -> [
				Rdb 1;
				Lit value;
				Bin Subtract;
				Bne end_label (* branch to failed match *)
			]
		| PatternBytes(bytes) ->
			List.flatten (List.map check_item bytes)
		| Binding(literal, bind_type) ->
			let num_bytes = Ast.size_of_bind_type bind_type in
			[
				Rdb num_bytes;
				(* TODO: store bytes into a binding variable *)
			]
	in
	List.flatten (List.map check_item expr);;


let rec translate env stmt =
	let recurse = translate env in
	match stmt with
	  Block(stmt_list) ->
		List.flatten (List.map recurse stmt_list)
	| Expr(expr) ->
		translate_expr env expr
	| Pattern(pat_expr, stmt) ->
		let end_label = get_new_label () in
		translated_pattern pat_expr end_label @
		recurse stmt @
		[Label end_label];;

let translate_program stmt =
	let env = clean_environment in
	resolve_labels (translate env stmt @ [Hlt]);;
