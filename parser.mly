%{
open Parser_help
%}

%token <Ast_types.bind_type> BIND_TYPE

/* punctuation */
%token COLON SEMICOLON COMMA FSLASH RBRACE LBRACE RPAREN LPAREN DEF IF ELSE

/* operators */
%token PLUS MINUS TIMES ASSIGN

/* comparators */
%token EQ NEQ LT LEQ GT GEQ

%token <string> STRING_LITERAL
%token <string> LITERAL
%token <string> INT_LITERAL
%token EOF

/* associativity and precedence */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES FSLASH

%start program
%type <Ast_types.statement> program

%%

pat_token:
	| INT_LITERAL { parse_pattern_const $1 }
	| LITERAL COLON BIND_TYPE { Ast_types.Binding($1, $3) }
	| LITERAL {
		(* Resolve a little ambiguity from the scanning phase, basically
		   binding variables can't be valid hex numbers or we don't know if it's
		   a hex number or a variable.  Here it's resolved to be a number. *)
		try parse_pattern_const $1
		with Failure(_) -> Ast_types.Literal($1)
		}
	| STRING_LITERAL { Ast_types.PatString($1) }

pat_expr:
	rev_pat_expr { List.rev $1 }

rev_pat_expr:
	| { [] }
	| rev_pat_expr pat_token { $2 :: $1 }

statement:
	| expr SEMICOLON { Ast_types.Expr($1) }
	| LBRACE statement_list RBRACE { Ast_types.Block($2) }
	| FSLASH pat_expr FSLASH statement { Ast_types.Pattern($2,$4) }
	| DEF LITERAL LPAREN lit_list RPAREN LBRACE statement_list RBRACE
		{ Ast_types.FunctionDecl({
			Ast_types.fname = $2;
			Ast_types.arguments = $4;
			Ast_types.body = Ast_types.Block($7)})
		}
	| IF LPAREN expr RPAREN statement %prec NOELSE
		{ Ast_types.If($3, $5, Ast_types.Block([])) }
	| IF LPAREN expr RPAREN statement ELSE statement
		{ Ast_types.If($3, $5, $7) }

expr:
	| INT_LITERAL { Ast_types.LitInt(int_of_string $1) }
	| expr PLUS   expr { Ast_types.Binopt($1, Ast_types.Add, $3) }
	| expr MINUS  expr { Ast_types.Binopt($1, Ast_types.Subtract, $3) }
	| expr TIMES  expr { Ast_types.Binopt($1, Ast_types.Multiply, $3) }
	| expr FSLASH expr { Ast_types.Binopt($1, Ast_types.Divide, $3) }
	| expr EQ     expr { Ast_types.Binopt($1, Ast_types.Equal, $3) }
	| expr NEQ    expr { Ast_types.Binopt($1, Ast_types.Neq, $3) }
	| expr LT     expr { Ast_types.Binopt($1, Ast_types.Less, $3) }
	| expr LEQ    expr { Ast_types.Binopt($1, Ast_types.Leq, $3) }
	| expr GT     expr { Ast_types.Binopt($1, Ast_types.Greater, $3) }
	| expr GEQ    expr { Ast_types.Binopt($1, Ast_types.Geq, $3) }
	| LITERAL ASSIGN expr { Ast_types.Assign($1, $3) }
	| LITERAL { Ast_types.ExprLiteral($1) }
	| LITERAL LPAREN expr_list RPAREN { Ast_types.Call($1, $3) }
	| STRING_LITERAL { Ast_types.LitString ($1) }

expr_list:
	| rev_expr_list { List.rev $1 }

rev_expr_list:
	| { [] }
	| expr { [$1] }
	| rev_expr_list COMMA expr { $3 :: $1 }

statement_list:
	| rev_statement_list { List.rev $1 }

rev_statement_list:
	| { [] }
	| rev_statement_list statement { $2 :: $1 }

lit_list:
	| rev_lit_list { List.rev $1 }

rev_lit_list:
	| { [] }
	| LITERAL { [$1] }
	| rev_lit_list COMMA LITERAL { $3 :: $1 }

program:
	statement_list { Ast_types.Block($1) }
