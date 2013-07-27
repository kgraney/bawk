(** Bytecode generation *)

(** [execute_bytecode] is a function called from the main argument parser.  It
	generates and then executes the bytecode instructions. *)
val execute_bytecode: Bytecode_types.instruction list -> in_channel -> unit

(** [print_bytecode] is a function called from the main argument parser.  It
	generates and then prints out the bytecode instructions. *)
val print_bytecode: Bytecode_types.instruction list -> unit

(** [string_of_instruction] converts a bytecode instruction into a string
	representation *)
val string_of_instruction: Bytecode_types.instruction -> string

(** [execute_instructions] interprets an array of bytecode instructions
	as a program, executing said instructions. *)
val execute_instructions: Bytecode_types.instruction array -> in_channel -> unit

(** [is_pseudo] returns true if the given bytecode instruction is a pseudo
	instruction and false otherwise *)
val is_pseudo: Bytecode_types.instruction -> bool

val enumerate_instructions: Bytecode_types.instruction list ->
	(int * Bytecode_types.instruction) list