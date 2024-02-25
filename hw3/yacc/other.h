#include <stdio.h>
#include <string.h>
#include <stdlib.h>
FILE *inputFile,*fileOutput;


typedef struct
{
    int pay;
    int payda;
}fraction_t;

/*This Linked List is to keep variables for set operation*/
typedef struct node_s
{
    fraction_t data;
    char name[30];
    struct node_s *next;
}node_t;

typedef struct
{
    enum { VALUE, OPERATION, FUNCTION_CALL } type;
    union {
        fraction_t value;

        struct {
            struct ExprNode* left;
            struct ExprNode* right;
            char op; // Operator like '+', '-', '*', '/'
        } operation;

        struct {
            char* functionName;
            struct ExprNode** arguments;
            int argCount;
        } functionCall;
    } data;
} ExprNode;

typedef struct
{
    char *name;
    char **parameters;
    int paramCount;
    ExprNode *body; // The body of the function is an expression
} Function_t;


void addNewVariable(char name[30], fraction_t data);

fraction_t getDataOfVariable(char name[30]);

fraction_t* appendElementToList(fraction_t *list, fraction_t element);

fraction_t* concatTwoList(fraction_t *list1, fraction_t *list2);

fraction_t convertToPayPayda(char str[30]);

char* convertToString(fraction_t fraction);

fraction_t simplifyFraction(fraction_t fraction);

fraction_t addTwoFraction(fraction_t fraction1, fraction_t fraction2);

fraction_t subtractTwoFraction(fraction_t fraction1, fraction_t fraction2);

fraction_t multiplyTwoFraction(fraction_t fraction1, fraction_t fraction2);

fraction_t divideTwoFraction(fraction_t fraction1, fraction_t fraction2);

int compareTwoFraction(fraction_t fraction1, fraction_t fraction2);

void printFraction(fraction_t fraction);

char* defineZeroArgumentFunction(char* name, fraction_t function);

char* defineOneArgumentFunction(char* name, fraction_t argument, char function[30]);

char* defineTwoArgumentsFunction(char* name, fraction_t argument1, fraction_t argument2, char function[30]);

fraction_t evaluateOneArgumentFunction(char* name, fraction_t argument);

fraction_t evaluateTwoArgumentsFunction(char* name, fraction_t argument1, fraction_t argument2);

// int zeroArgumentFunction()