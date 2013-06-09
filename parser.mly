%{ %}

%token INT_1_BYTE INT_2_BYTES INT_4_BYTES
%token UINT_1_BYTE UINT_2_BYTES UINT_4_BYTES

%token COLON SEMICOLON FSLASH RBRACE LBRACE

%token <int> LITERAL
%token EOF

%start program
%type <Ast_types.program> program

%%

pat_expr:
	| { [] }
	| pat_expr expr { $2 :: $1 }

statement:
	| expr SEMICOLON { Ast_types.Expr($1) }
	| LBRACE statement_list RBRACE { Ast_types.Block($2) }
	| FSLASH pat_expr FSLASH statement { Ast_types.Pattern($2,$4) }

expr:
	| LITERAL { Ast_types.Lit($1) }

statement_list:
	| { [] }
	| statement_list statement { $2 :: $1 }

program:
	statement_list { $1 }
