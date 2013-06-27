

(** [parse_pattern_const] converts a pattern constant into a [PatternBytes]
	list of integers for the AST.  This method interprets leading zeros as
	significant among other things. *)
val parse_pattern_const: string -> Ast_types.pat_token