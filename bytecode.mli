(** Bytecode generation *)

(** [execute_bytecode] is a function called from the main argument parser.  It
	generates and then executes the bytecode instructions. *)
val execute_bytecode: Ast_types.statement -> unit

(** [print_bytecode] is a function called from the main argument parser.  It
	generates and then prints out the bytecode instructions. *)
val print_bytecode: Ast_types.statement -> unit

(** [string_of_instruction] converts a bytecode instruction into a string
	representation *)
val string_of_instruction: Bytecode_types.instruction -> string