(** Types used by the AST *)

type operators = Add | Subtract | Multiply | Divide
	| Equal | Neq | Less | Leq | Greater | Geq

type bind_type =
	| Int_1_byte
	| Int_2_bytes
	| Int_4_bytes
	| UInt_1_byte
	| UInt_2_bytes
	| UInt_4_bytes

type literal = string

type expr =
	| LitInt of int
	| ExprLiteral of string
	| Binopt of expr * operators * expr
	| Call of string * expr list
	| LitString of string
	| Assign of string * expr

type pat_token =
	| PatternByte of int
	| PatternBytes of pat_token list
	| Binding of literal * bind_type
	| Literal of literal
	| PatString of string

type pat_expr = pat_token list

type func_decl = {
	fname: string;
	arguments: string list;
	body: statement;
}
and statement =
	| Pattern of pat_expr * statement
	| Block of statement list
	| Expr of expr
	| FunctionDecl of func_decl
	| If of expr * statement * statement
