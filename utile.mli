(** General purpose utility functions *)


(** [int_of_hex] converts a string of hexadecimal digits into an integer *)
val int_of_hex: string -> int

(** [enumerate] converts a list of values in a list of tuples of an integer
	followed by that value.  Integers begin at zero and count upwards *)
val enumerate: ?step:('a -> int -> int) -> 'a list -> (int * 'a) list

(** [explode] converts a string into a list of characters *)
val explode: string -> char list

(** [implode] converts a list of characters into a string *)
val implode: char list -> string
