(** Compilation of bawk code *)

(** [translate] takes an AST statement and returns a list of bytecode
	instructions implementing the statement *)
val translate: Ast_types.statement -> Bytecode_types.instruction list
