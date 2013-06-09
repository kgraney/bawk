{ open Parser }

let hex_character = ['0'-'9' 'A'-'F' 'a'-'f']
let hex_sequence = hex_character+

rule token = parse
	| [' ' '\t' '\r' '\n'] { token lexbuf }
	| "/*" { comment lexbuf }

	(* language semantics *)
	| ":" { COLON }
	| "/" { FSLASH }
	| "{" { LBRACE }
	| "}" { RBRACE }

	(* type definitions *)
	| "int1" { INT_1_BYTE }
	| "int2" { INT_2_BYTES }
	| "int4" { INT_4_BYTES }
	| "uint2" { UINT_2_BYTES }
	| "uint4" { UINT_4_BYTES }


	| hex_sequence as lit { LITERAL(Utile.int_of_hex lit) }
	| eof { EOF }


and comment = parse
	| "*/" { token lexbuf }
	| _ { comment lexbuf }
