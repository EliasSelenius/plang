#pragma once

#include "types.h"

// TODO: inluded due to StrSpan. move StrSpan into own file, or create string library
#include "plang-lexer.h"

typedef struct PlangType {
    StrSpan structName;
    u8 numPointers;
} PlangType;

typedef struct PlangFunction {
    StrSpan name;
    PlangType returnType;
} PlangFunction;

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

typedef struct VarDecl {
    PlangType type;
    Expression* assignmentOrNull;
} VarDecl;


typedef enum StatementType {
    Statement_Declaration,
    Statement_Assignment,
    
    Statement_If,
    Statement_While
}

typedef struct Statement {
    StatementType statementType;
    void* node;
} Statement;


typedef struct Codeblock {
    Statement* statements; // list
} Codeblock;


Expression* parseExpression();
