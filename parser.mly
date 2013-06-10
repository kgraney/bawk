%{ %}

%token <Ast_types.bind_type> BIND_TYPE

%token COLON SEMICOLON FSLASH RBRACE LBRACE

%token <string> LITERAL
%token <int> INT_LITERAL
%token EOF

%start program
%type <Ast_types.statement> program

%%

pat_token:
	| INT_LITERAL { Ast_types.Lit($1) }
	| LITERAL COLON BIND_TYPE { Ast_types.Binding($1, $3) }

pat_expr:
	| { [] }
	| pat_expr pat_token { $2 :: $1 }

statement:
	| expr SEMICOLON { Ast_types.Expr($1) }
	| LBRACE statement_list RBRACE { Ast_types.Block($2) }
	| FSLASH pat_expr FSLASH statement { Ast_types.Pattern($2,$4) }

expr:
	| INT_LITERAL { Ast_types.LitInt($1) }

statement_list:
	| { [] }
	| statement_list statement { $2 :: $1 }

program:
	statement_list { Ast_types.Block($1) }
