{ open Parser }

let hex_character = ['0'-'9' 'A'-'F' 'a'-'f']
let hex_sequence = "0x" hex_character+
let literal = ['A'-'Z' 'a'-'z']['0'-'9' 'A'-'Z' 'a'-'z' '_']*

rule token = parse
	| [' ' '\t' '\r' '\n'] { token lexbuf }
	| "/*" { comment lexbuf }

	(* language semantics *)
	| ':'  { COLON }
	| ';'  { SEMICOLON }
	| '/'  { FSLASH }
	| '{'  { LBRACE }
	| '}'  { RBRACE }
	| '('  { LPAREN }
	| ')'  { RPAREN }
	| '+'  { PLUS }
	| '-'  { MINUS }
	| '*'  { TIMES }
	| '/'  { DIVIDE }
	| '='  { ASSIGN }
	| "==" { EQ }
	| "!=" { NEQ }
	| '<'  { LT }
	| "<=" { LEQ }
	| '>'  { GT }
	| ">=" { GEQ }

	(* type definitions *)
	| "int1" { BIND_TYPE(Ast_types.Int_1_byte) }
	| "int2" { BIND_TYPE(Ast_types.Int_2_bytes) }
	| "int4" { BIND_TYPE(Ast_types.Int_4_bytes) }
	| "uint2" { BIND_TYPE(Ast_types.UInt_2_bytes) }
	| "uint4" { BIND_TYPE(Ast_types.UInt_4_bytes) }

	| literal as lit { LITERAL(lit) }

	| hex_sequence as lit { INT_LITERAL(int_of_string lit) }
	| eof { EOF }


and comment = parse
	| "*/" { token lexbuf }
	| _ { comment lexbuf }
