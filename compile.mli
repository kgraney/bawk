(** Compilation of bawk code *)

(** [translate_program] takes an AST statement and returns a list of bytecode
	instructions implementing the statement *)
val translate_program: Ast_types.statement -> Bytecode_types.instruction list
