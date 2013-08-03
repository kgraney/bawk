
let read_byte ic =
	let byte = input_char ic in
	int_of_char byte;;

let read_bytes ic n =
	let rec read c lst =
		if c == 0 then List.rev lst
		else read (c - 1) (read_byte ic :: lst)
	in read n [];;

let read_string_null ic =
	let rec scan ic str =
		let c = input_char ic in
		match c with
			| '\000' -> Utile.implode (List.rev str)
			| _ -> scan ic (c :: str)
	in scan ic [];;

let read_string_fixed ic n =
	let rec scan ic str n =
		if n == 0 then Utile.implode (List.rev str)
		else
			let c = input_char ic in
			scan ic (c :: str) (n - 1)
	in scan ic [] n;;

let read_unsigned ic n =
	let bytes = read_bytes ic n in
	let rbytes = List.rev bytes in
	let rec form_int n shift bytes =
		match bytes with
			| [] -> n
			| h :: t ->
				let next_n = n + (h lsl shift) in
				form_int next_n (shift + 8) t
	in form_int 0 0 rbytes;;

let advance ic n =
	let pos = pos_in ic in
	let new_pos = pos + n in
	seek_in ic new_pos;;

let get_pos ic =
	pos_in ic;;

let set_pos ic n =
	seek_in ic n;;

let is_eof ic =
	let pos = get_pos ic in
	let result = try input_char ic; false with End_of_file -> true in
	set_pos ic pos; result;;
