open Ast_types
open Bytecode_types

module StringMap = Map.Make(String)
exception Compile_error of string;;

type pattern_binding = {
	loc: int;
	size: int;
}

type env = {
	symbol_map: int StringMap.t;
	bindings: pattern_binding StringMap.t;
	parent: env ref option;
}

let built_in_functions = ["print"; "RP"];;

let clean_environment =
	let built_ins =
		List.fold_left (fun map item ->
			let (value, key) = item in
			StringMap.add key (value - 1) map
		) StringMap.empty (Utile.enumerate ~step:(fun x y -> y - 1)
				built_in_functions) in
	{
		symbol_map = built_ins;
		bindings = StringMap.empty;
		parent = None;
	}

let new_environment parent_env =
	ref {
		symbol_map = StringMap.empty;
		bindings = StringMap.empty;
		parent = Some parent_env;
	};;

let rec resolve_symbol env name =
	try StringMap.find name !env.symbol_map
	with Not_found ->
		match !env.parent with
		  None -> let error_msg = Printf.sprintf "No such symbol: %s" name in
			raise (Compile_error error_msg)
		| Some(env) -> resolve_symbol env name;;

let add_function env fname addr =
	let symbol_map_new = StringMap.add fname addr env.symbol_map in
	{env with symbol_map = symbol_map_new}

let global_counter = ref 0;;
let get_next_global () =
	global_counter := !global_counter + 1;
	!global_counter;;

let add_variable_force env vname id =
	let symbol_map_new = StringMap.add vname id
			env.symbol_map in
	{env with symbol_map = symbol_map_new}

let add_variable env vname =
	let symbol_map_new = StringMap.add vname (get_next_global ())
			env.symbol_map in
	{env with symbol_map = symbol_map_new}

let rec get_var_address env vname =
	try StringMap.find vname !env.symbol_map;
	with Not_found ->
		match !env.parent with
			  None -> env := add_variable !env vname;
				StringMap.find vname !env.symbol_map
			| Some(env) -> get_var_address env vname;;

let add_binding env bname size =
	let vaddr = get_var_address env bname in
	(* let symbol_map_new = StringMap.add bname vaddr env.symbol_map in *)
	let bindings_new = StringMap.add bname { loc = vaddr; size = size}
			!env.bindings in
	{!env with bindings = bindings_new}

exception Bad_binding of string;;

let get_binding_address env bname size =
	let bind_info =
	try StringMap.find bname !env.bindings;
	with Not_found ->
		env := add_binding env bname size;
		StringMap.find bname !env.bindings
	in
	if bind_info.size != size then
		raise (Bad_binding "size mismatch")
	else bind_info.loc;;

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
		| Bra(id)::t -> Bra label_map.(id) :: emit_resolution t
		| Beq(id)::t -> Beq label_map.(id) :: emit_resolution t
		| Beo(id)::t -> Beo label_map.(id) :: emit_resolution t
		| Jsr(id)::t when id >= 0 -> Jsr label_map.(id) :: emit_resolution t

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
	| ExprLiteral(var_name) ->
		let vaddr = resolve_symbol env var_name in
		[ Lod vaddr ]
	| Binopt(e1, op, e2) ->
		recurse e1 @ recurse e2 @ [Bin op]
	| Call(func_name, args) ->
		let function_addr = resolve_symbol env func_name in
		(List.concat (List.map recurse args)) @ [Jsr function_addr]
	| Assign(var_name, expr) ->
		let vaddr = get_var_address env var_name in
		translate_expr env expr @ [
			Str vaddr
		]

and translated_pattern env expr fail_label =
	let rec check_item = function
		  PatternByte(value) -> [
				Rdb 1;
				Lit value;
				Bin Subtract;
				Bne fail_label (* branch to failed match *)
			]
		| PatternBytes(bytes) ->
			List.flatten (List.map check_item bytes)
		| Binding(literal, bind_type) ->
			let num_bytes = Ast.size_of_bind_type bind_type in
			let vaddr = get_binding_address env literal num_bytes in
			[
				(* TODO: handle EOF here? *)
				Rdb num_bytes;
			]
			@ (
				if Ast.is_signed_type bind_type then [Two (num_bytes * 8);]
				else []
			)
			@ [ Str vaddr; ]
		| PatString(str) ->
			let bytes = Utile.bytes_of_string str in
			let ast_bytes = List.map (fun x -> PatternByte x) bytes in
			translated_pattern env ast_bytes fail_label @ [];
	in
	List.flatten (List.map check_item expr)

and translate env stmt =
	let recurse = translate env in
	match stmt with
	  Block(stmt_list) ->
		List.flatten (List.map recurse stmt_list)
	| Expr(expr) ->
		translate_expr env expr
	| Pattern(pat_expr, stmt) ->
		let start_label = get_new_label () in
		let fail_label = get_new_label () in
		let end_label = get_new_label () in

		let new_env = new_environment env in

		let pattern_code = translated_pattern new_env pat_expr fail_label in
		[ Ldp; Label start_label; Ldp; Beo end_label; ] @ pattern_code @
		translate new_env stmt @
		[
			Bra end_label;
			Label fail_label;
			Skp;
			Beo end_label;
			Rdb 1; Drp;
			Bra start_label;
			Label end_label;
			Skp;
			Skp;
		]
	| FunctionDecl(decl) ->
		let start_label = get_new_label () in
		let end_label = get_new_label () in
		let num_args = List.length decl.arguments in
		let new_env = new_environment env in
		List.iter (fun x ->
			let (id,name) = x in
			new_env := add_variable_force !new_env name id;
			();
		) (Utile.enumerate ~step:(fun x y -> y - 1) ~start:(-100)
				(List.rev decl.arguments));
		env := add_function !env decl.fname start_label;
		[ Bra end_label; Label start_label; Ent; ] @
		translate new_env decl.body @
		[ Rts num_args; Label end_label ]
	| If(condition, ifstmt, elsestmt) ->
		let if_label = get_new_label () in
		let end_label = get_new_label () in
		translate_expr env condition
		@ [ Bne if_label; ] @ translate env elsestmt
		@ [ Bra end_label; Label if_label; ]
		@ translate env ifstmt @ [ Label end_label; ];;

let translate_program stmt =
	let env = ref clean_environment in
	resolve_labels (translate env stmt @ [Hlt]);;
