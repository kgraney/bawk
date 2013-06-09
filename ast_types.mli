
type expr =
	| Lit of int

type pat_expr = expr list

type statement =
	| Pattern of pat_expr * statement
	| Block of statement list
	| Expr of expr

type program = statement list
