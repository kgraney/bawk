(** Types used by the AST *)

type operators = Add | Subtract | Multiply | Divide

type bind_type =
	| Int_1_byte
	| Int_2_bytes
	| Int_4_bytes
	| UInt_2_bytes
	| UInt_4_bytes

type literal = string

type expr =
	| LitInt of int
	| Binopt of expr * operators * expr
	| Call of string * expr list

type pat_token =
	| Lit of int
	| Binding of literal * bind_type
	| Literal of literal

type pat_expr = pat_token list

type statement =
	| Pattern of pat_expr * statement
	| Block of statement list
	| Expr of expr
