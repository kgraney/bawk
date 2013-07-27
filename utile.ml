

let int_of_hex str =
	int_of_string ("0x" ^ str);;

let enumerate ?step:(step=(fun x y -> y + 1)) lst =
	let rec enum count = function
		  [] -> []
		| h :: t -> (count, h) :: (enum (step h count) t)
	in enum 0 lst;;
