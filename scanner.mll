{ open Parser

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let is_in_string = ref false
let in_string () = !is_in_string

let reset_string_buffer () =
	string_buff := initial_string_buffer;
	string_index := 0

let store_string_char c =
	if !string_index >= String.length (!string_buff) then
	begin
		let new_buff = String.create (String.length (!string_buff) * 2) in
			String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
			string_buff := new_buff
	end;
	String.unsafe_set (!string_buff) (!string_index) c;
	incr string_index

let store_lexeme lexbuf =
	let s = Lexing.lexeme lexbuf in
	for i = 0 to String.length s - 1 do
		store_string_char s.[i];
	done

let get_stored_string () =
	let s = String.sub (!string_buff) 0 (!string_index) in
	string_buff := initial_string_buffer;
	s

let char_for_backslash = function
	| 'n' -> '\010'
	| 'r' -> '\013'
	| 'b' -> '\008'
	| 't' -> '\009'
	| c -> c

}

let hex_character = ['0'-'9' 'A'-'F' 'a'-'f']
let hex_sequence = "0x" hex_character+
let hexdigit_sequence = hex_character+
let literal = ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*

let newline = ('\010' | "\013\010")

rule token = parse
	| [' ' '\t' '\r' '\n'] { token lexbuf }
	| "/*" { comment lexbuf }

	(* language semantics *)
	| ':'  { COLON }
	| ';'  { SEMICOLON }
	| ','  { COMMA }
	| '/'  { FSLASH }
	| '{'  { LBRACE }
	| '}'  { RBRACE }
	| '('  { LPAREN }
	| ')'  { RPAREN }
	| '+'  { PLUS }
	| '-'  { MINUS }
	| '*'  { TIMES }
	  (* DIVIDE is the same as FSLASH *)
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

	| "def" { DEF }
	| literal as lit { LITERAL(lit) }

	| hexdigit_sequence as lit { INT_LITERAL(lit)}
	| hex_sequence as lit { INT_LITERAL(lit) }

	(* string literals *)
	| "\"" { reset_string_buffer();
			is_in_string := true;
			let string_start = lexbuf.Lexing.lex_start_p in
			string lexbuf;
			is_in_string := false;
			lexbuf.Lexing.lex_start_p <- string_start;
			STRING_LITERAL(get_stored_string()) }

	| eof { EOF }


and comment = parse
	| "*/" { token lexbuf }
	| _ { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { string lexbuf }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | eof
      { is_in_string := false;
      	assert false
        (*raise (Error (Unterminated_string, 0))*) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

