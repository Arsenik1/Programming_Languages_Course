%{
#include "other.h"
#include "y.tab.h"
%}

%%
and { return KW_AND; }
or { return KW_OR; }
not { return KW_NOT; }
equal { return KW_EQUAL; }
less { return KW_LESS; }
nil { return KW_NIL; }
list { return KW_LIST; }
append { return KW_APPEND; }
concat { return KW_CONCAT; }
set { return KW_SET; }
def { return KW_DEF; }
for { return KW_FOR; }
if { return KW_IF; }
exit { return KW_EXIT; }
load { return KW_LOAD; }
display { return KW_DISPLAY; }
true { return KW_TRUE; }
false { return KW_FALSE; }
"+" { return OP_PLUS; }
"-" { return OP_MINUS; }
"/" { return OP_DIV; }
"*" { return OP_MULT; }
"(" { return OP_OP; }
")" { return OP_CP; }
"," { return OP_COMMA; }
[0-9]+"b"[0-9]+  { char* str = strdup(yytext);
					fraction_t result;
					result.pay = atoi(strtok(str, "b"));
					result.payda = atoi(strtok(NULL, "b"));
					yylval.fraction = result;
					return VALUEF; }
";;"[^\n]* { return COMMENT; }
[ \t\r\n]+ {}
[a-zA-Z][a-zA-Z0-9]* { yylval.str = strdup(yytext); return IDENTIFIER; }

%%
// . { return SYNTAX_ERRORcannot be tokenized\n", yytext); }
// liste { return SYNTAX_ERRORte cannot be tokenized\n"); }

// int main() {
//     yylex(); // Call the lexer to start tokenizing input.
//     return 0;
// }
