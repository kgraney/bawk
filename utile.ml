

let int_of_hex str =
	int_of_string ("0x" ^ str);;

let enumerate ?step:(step=(fun x y -> y + 1)) lst =
	let rec enum count = function
		  [] -> []
		| h :: t -> (count, h) :: (enum (step h count) t)
	in enum 0 lst;;

let explode s =
	let rec exp i l =
		if i < 0 then l else exp (i - 1) (s.[i] :: l) in
	exp (String.length s - 1) [];;

let implode l =
	let res = String.create (List.length l) in
	let rec imp i = function
	| [] -> res
	| c :: l -> res.[i] <- c; imp (i + 1) l in
	imp 0 l;;

let bytes_of_string str =
	let chars = explode str in
	List.map int_of_char chars;;
