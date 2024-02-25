%{
#include "other.h"
#include <stdio.h>
#include <stdlib.h>
extern FILE *yyin;
extern int yylex(void);
void yyerror(const char *s);
%}

%union{
    fraction_t fraction;
    char* str;
    Function_t Function;
}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISPLAY KW_TRUE KW_FALSE

%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA

%token COMMENT
%token <fraction> VALUEF
%token <str> IDENTIFIER

%type <fraction> EXPR
%type <Function> FUNCTION
%type <Function> FUNCTION_DEF FUNCTION_CALL

%%
    program:
        program START
        |
        START
        ;

    START:
        EXPR 
        |  
        FUNCTION 
        | 
        OP_OP KW_EXIT OP_CP
        ;
    
    EXPR: 
        OP_OP OP_PLUS EXPR EXPR OP_CP { fraction_t f = addTwoFraction($3, $4); printFraction(f); $$ = f;}
        | 
        OP_OP OP_MINUS EXPR EXPR OP_CP { fraction_t f = subtractTwoFraction($3, $4); printFraction(f); $$ = f;}
        | 
        OP_OP OP_MULT EXPR EXPR OP_CP { fraction_t f = multiplyTwoFraction($3, $4); printFraction(f); $$ = f;}
        | 
        OP_OP OP_DIV EXPR EXPR OP_CP { fraction_t f = divideTwoFraction($3, $4); printFraction(f); $$ = f;}
        |
        OP_OP IDENTIFIER EXPR OP_CP { /*$$ = evaluateOneArgumentFunction($2, $3);*/ fprintf(fileOutput, "evaluateOneArgumentFunction\n"); }
        |
        OP_OP IDENTIFIER EXPR EXPR OP_CP { /*$$ = evaluateTwoArgumentsFunction($2, $3, $4);*/ fprintf(fileOutput, "evaluateTwoArgumentsFunction\n"); }
        | 
        OP_OP IDENTIFIER EXPR EXPR EXPR OP_CP { } /*işlevsiz ama syntax OK.*/
        |
        OP_OP KW_SET IDENTIFIER EXPR OP_CP {$$ = $4; addNewVariable($3, $4); fprintf(fileOutput, "addNewVariable\n");} /*işlevsiz ama syntax OK.*/
        |
        IDENTIFIER { $$ = getDataOfVariable($1);}
        |
        VALUEF { $$ = $1; /*printFraction($1);*/}

    FUNCTION:
        OP_OP KW_DEF IDENTIFIER EXPR OP_CP { /*$$ = defineZeroArgumentFunction($3, $4);*/ fprintf(fileOutput, "defineZeroArgumentFunction\n");} /*işlevsiz ama syntax OK.*/
        |
        OP_OP KW_DEF IDENTIFIER IDENTIFIER EXPR OP_CP { /*$$ = defineOneArgumentFunction($3, $4, $5);*/ fprintf(fileOutput, "defineOneArgumentFunction\n"); }
        |
        OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXPR OP_CP { /*$$ = defineTwoArgumentsFunction($3, $4, $5, $6);*/ fprintf(fileOutput, "defineTwoArgumentsFunction\n"); }
        ;
   
%%
void yyerror(const char *msg) {
    fprintf(stderr, "Error: %s\n", msg);
}

int main(int argc, char **argv){
    fileOutput = fopen("output_parsed.txt", "w");
    if (!fileOutput) {
        printf("Error: output_parsed.txt cannot be opened!\n");
        exit(1);
    }

    if (argc > 1) {
        inputFile = fopen(argv[1], "r");
        if (!inputFile) {
            printf("Error: Input file not found!\n");
            fclose(fileOutput); // Close the output file if input file not found
            exit(1);
        }
        yyin = inputFile;
    } else {
        printf("Interactive mode activated. Type 'exit' to quit.\n> ");
        char *buffer = NULL;
        char *inputLine = (char *)malloc(20 * sizeof(char));
        size_t bufferSize = 0;
        int lineLength;

        while ((lineLength = getline(&buffer, &bufferSize, stdin)) != 1) {
            inputLine = (char *)realloc(inputLine, (strlen(inputLine) + bufferSize + 1) * sizeof(char));
            strcat(inputLine, buffer);
        }

        FILE *stream = fmemopen(inputLine, strlen(inputLine) - 1, "r");
        yyin = stream;
    }

    yyparse();
    printf("Parsing completed.\n");

    // Close the files
    fclose(fileOutput);
    if (argc > 1) {
        fclose(inputFile);
    }

    exit(0);
}


