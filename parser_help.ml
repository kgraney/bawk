
let split_bytes s =
	let split s =
		let num_bytes = (String.length s) / 2 in
		let arr = Array.make num_bytes (String.create 2) in
		for i = 0 to num_bytes - 1 do
			arr.(i) <- String.create 2;
			String.blit s (2*i) arr.(i) 0 2
		done;
		Array.to_list arr
	in
	let pieces =
		if (String.length s) mod 2 == 0 then split s
		else split (Printf.sprintf "0%s" s)
	in
	List.map (fun x -> int_of_string ("0x" ^ x)) pieces;;

let parse_pattern_const s =
	let lst = split_bytes s in
	let bytes = List.map (fun x -> Ast_types.PatternByte x) lst in
	Ast_types.PatternBytes bytes