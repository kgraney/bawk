
let parse_pattern_const s =
	let lst = [0; 1; 2; 3;] in (* TODO: implement conversion *)
	let bytes = List.map (fun x -> Ast_types.PatternByte x) lst in
	Ast_types.PatternBytes bytes