(** The Reader module contains functions for extracting and decoding bits of information
	from an [in_channel] for a given binary file. *)


(** [read_byte] reads a single byte from the input channel and returns an integer
	representing that byte *)
val read_byte: in_channel -> int

(** [read_bytes] reads the next [n] bytes from the input channel into a list of integers *)
val read_bytes: in_channel -> int -> int list

(** [read_string_null] reads a null-terminated string from the input channel *)
val read_string_null: in_channel -> string

(** [read_string_fixed] reads a fixed-length string from the input channel *)
val read_string_fixed: in_channel -> int -> string

(** [read_unsigned] reads the next [n] bytes as an unsigned integer

	The algorithm is: read bytes into a list with [read_bytes], traverse the list in reverse
	order, and accumulate a sum of every byte shifted by it's position in the list. *)
val read_unsigned: in_channel -> int -> int

(** [get_pos] returns the position in the file *)
val get_pos: in_channel -> int

(** [set_pos] set the position in the file *)
val set_pos: in_channel -> int -> unit

val advance: in_channel -> int -> unit

(** [is_eof] returns true if the next byte of the file reaches EOF and false
	otherwise *)
val is_eof: in_channel -> bool
