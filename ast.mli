

(** [print_tree] outputs the AST in GraphViz DOT format.  This is useful for
	visualizing how the parser is constructing the tree *)
val print_tree: Ast_types.statement -> unit

(** [string_of_bind_type] converts an [Ast_types.bind_type] to a [string] *)
val string_of_bind_type: Ast_types.bind_type -> string

(** [size_of_bind_type] returns the size, in bytes, of an [Ast_types.bind_type]
	*)
val size_of_bind_type: Ast_types.bind_type -> int

(** [make_terminal] creates a terminal node for the node with an id of [this],
	and labels it with the string argument.  If the optional [parent] argument
	is provided a link is also created between this node and the parent. *)
val make_terminal: this:int -> ?parent:int -> string -> unit

(** [make_nonterminal] creates a non-terminal node for the node with an id of
	[this], and labels it with the string argument.  If the optional [parent]
	argument is provided a link is also created between this node and the
	parent. *)
val make_nonterminal: this:int -> ?parent:int -> string -> unit

(** [make_link] creates a link between two of the GraphViz nodes *)
val make_link: int -> int -> unit

(** [folded_printer] returns a function that can be used with [List.fold_left]
	to traverse a list of objects that should be children of a common parent.
	The parent's ID is the second argument and the print function is the 
	first argument. *)
val folded_printer: ('a -> int -> int -> 'b) -> int -> int -> 'a -> 'b

(** [string_of_operator] returns a string of the bytecode instruction for
	the given operator. *)
val string_of_operator: Ast_types.operators -> string