%{ %}

%token INT_1_BYTE INT_2_BYTES INT_4_BYTES
%token UINT_1_BYTE UINT_2_BYTES UINT_4_BYTES

%token COLON FSLASH

%token <int> LITERAL
%token EOF

%start expr
%type <Ast.expr> expr

%%

expr:
	| LITERAL { Ast.Lit($1) }

