
type instruction =
	  Lit of int    (* Push a literal *)
	| Drp           (* Discard a value *)
	| Bin of Ast_types.operators (* Perform arithmetic on top of stack *)
	| Lod of int    (* Fetch global variable *)
	| Str of int    (* Store global variable *)
	| Lfp of int    (* Load frame pointer relative *)
	| Sfp of int    (* Store frame pointer relative *)
	| Jsr of int    (* Call function by absolute address *)
	| Ent of int    (* Push FP, FP -> SP, SP += i *)
	| Rts of int    (* Restore FP, SP, consume formals, push result *)
	| Beq of int    (* Branch relative if top-of-stack is zero *)
	| Bne of int    (* Branch relative if top-of-stack is non-zero *)
	| Bra of int    (* Branch relative *)
	| Hlt           (* Terminate *)
	| Rdb of int    (* Read a number of bytes from the file *)
	| Ldp           (* Load the position in the file onto the stack *)
	| Skp           (* Seek the file to the position on the stack *)

	| Label of int  (* Pseudo bytecode instruction used by compiler *)