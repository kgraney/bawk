%{ %}

%token <Ast_types.bind_type> BIND_TYPE

/* punctuation */
%token COLON SEMICOLON COMMA FSLASH RBRACE LBRACE RPAREN LPAREN

/* operators */
%token PLUS MINUS TIMES ASSIGN

/* comparators */
%token EQ NEQ LT LEQ GT GEQ

%token <string> LITERAL
%token <int> INT_LITERAL
%token EOF

/* associativity and precedence */
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES FSLASH

%start program
%type <Ast_types.statement> program

%%

pat_token:
	| INT_LITERAL { Ast_types.Lit($1) }
	| LITERAL COLON BIND_TYPE { Ast_types.Binding($1, $3) }
	| LITERAL { Ast_types.Literal($1) }

pat_expr:
	| { [] }
	| pat_expr pat_token { $2 :: $1 }

statement:
	| expr SEMICOLON { Ast_types.Expr($1) }
	| LBRACE statement_list RBRACE { Ast_types.Block($2) }
	| FSLASH pat_expr FSLASH statement { Ast_types.Pattern($2,$4) }

expr:
	| INT_LITERAL { Ast_types.LitInt($1) }
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
	| LITERAL { Ast_types.ExprLiteral($1) }
	| LITERAL LPAREN expr_list RPAREN { Ast_types.Call($1, $3) }

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

program:
	statement_list { Ast_types.Block($1) }
