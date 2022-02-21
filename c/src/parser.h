#pragma once

#include "types.h"

#include "essh-string.h"

void parse();



typedef struct PlangType {
    StrSpan structName;
    u8 numPointers;
} PlangType;


// ----Expressions---------------------------------------------

typedef enum ExprType {
    ExprType_Number,
    ExprType_Variable,
    ExprType_Arithmetic
} ExprType;

// TODO: maybe create an expression type that have no need for unions
typedef struct Expression {
    ExprType expressionType;
    union {
        StrSpan* value;
        struct {
            u32 count;
            StrSpan* operators;
            struct Expression** subExpressions;
        };
    };
} Expression;



// ----Statements----------------------------------------------

typedef struct VarDecl {
    PlangType type;
    StrSpan name;
    Expression* assignmentOrNull;
} VarDecl;


typedef enum StatementType {
    Statement_Declaration,
    Statement_Assignment,
    
    Statement_If,
    Statement_While
} StatementType;

typedef struct Statement {
    StatementType statementType;
    void* node;
} Statement;


typedef struct Codeblock {
    struct Codeblock* parentScope;
    Statement* statements; // darray
} Codeblock;


typedef struct PlangFunction {
    StrSpan name;
    PlangType returnType;
    Codeblock scope;
} PlangFunction;

typedef struct Field {
    PlangType type;
    StrSpan name;
} Field;

typedef struct PlangStruct {
    StrSpan name;
    Field* fields; // darray
} PlangStruct;

extern PlangFunction* functions;
extern PlangStruct* structs;