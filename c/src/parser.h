#pragma once

#include "types.h"
#include "essh-string.h"
#include "lexer.h"

void parse();



typedef struct PlangType {
    StrSpan structName;
    u8 numPointers;
} PlangType;


// ----Expressions---------------------------------------------


typedef enum ExprType {
    ExprType_Number,
    ExprType_String,
    ExprType_Bool,
    ExprType_Variable,
    ExprType_Arithmetic,
    ExprType_Alloc,
    ExprType_Null,
    ExprType_Ternary
} ExprType;

// TODO: maybe create an expression type that have no need for unions
typedef struct Expression {
    ExprType expressionType;
    union {
        void* node;

        // TODO: make this a node
        StrSpan value;

        // TODO: make this a node
        struct {
            u32 count;
            StrSpan* operators;
            struct Expression** subExpressions;
        };
    };
} Expression;


typedef struct AllocExpression {
    Expression* sizeExpr;
    PlangType type;
} AllocExpression;


typedef struct ValuePath {
    StrSpan name;
    Expression* index;
    struct ValuePath* next;
} ValuePath;

typedef struct TernaryExpression {
    Expression* condition;
    Expression* thenExpr;
    Expression* elseExpr;
} TernaryExpression;

// ----Statements----------------------------------------------

typedef struct VarDecl {
    PlangType type;
    StrSpan name;
    bool mustInferType;
    Expression* assignmentOrNull;
} VarDecl;

typedef enum StatementType {
    Statement_Declaration,
    Statement_Assignment,
    
    Statement_If,
    Statement_While,

    Statement_Continue,
    Statement_Break,
    Statement_Return
} StatementType;

typedef struct Statement {
    StatementType statementType;
    void* node;
} Statement;


typedef struct Assignement {
    ValuePath* assignee;
    // = += -= *= /=
    TokenType assignmentOper;
    Expression* expr;
} Assignement;

typedef struct Codeblock {
    struct Codeblock* parentScope;
    Statement* statements; // darray
} Codeblock;

typedef struct WhileStatement {
    Expression* condition;
    Codeblock scope;
} WhileStatement;

typedef struct IfStatement {
    Expression* condition;
    Codeblock scope;
    struct IfStatement* next;
} IfStatement;

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