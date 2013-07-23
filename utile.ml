

let int_of_hex str =
	int_of_string ("0x" ^ str);;

let enumerate lst =
	let rec enum count = function
		  [] -> []
		| h :: t -> (count, h) :: (enum (count + 1) t)
	in enum 0 lst;;
