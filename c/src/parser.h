#pragma once

#include "types.h"
#include "essh-string.h"
#include "lexer.h"

u32 parse();

typedef struct PlangFunction PlangFunction;
typedef struct FuncDeclaration FuncDeclaration;

typedef struct Node {
    u32 lineNumber;
} Node;

typedef struct PlangType {
    StrSpan structName;
    u8 numPointers;
} PlangType;


// ----Expressions---------------------------------------------


typedef enum ExprType {
    ExprType_Number_Literal,
    ExprType_String_Literal,
    ExprType_Bool_Literal,
    ExprType_Variable,
    ExprType_Arithmetic,
    ExprType_Alloc,
    ExprType_Null,
    ExprType_Ternary,
    ExprType_FuncCall
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
    
    /*
        the type of the variable that name refers to.
    */
    PlangType* type;

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


typedef enum StatementType {
    Statement_Declaration,
    Statement_Assignment,
    Statement_FuncCall,

    Statement_If,
    Statement_While,

    Statement_Continue,
    Statement_Break,
    Statement_Return
} StatementType;

typedef struct Statement {
    Node nodebase;
    StatementType statementType;
} Statement;

typedef struct VarDecl {
    Statement base;
    PlangType type;
    StrSpan name;
    bool mustInferType;
    Expression* assignmentOrNull;
} VarDecl;

typedef struct Assignement {
    Statement base;
    ValuePath* assignee;
    // = += -= *= /=
    TokenType assignmentOper;
    Expression* expr;
} Assignement;

typedef struct Codeblock {
    struct Codeblock* parentScope;
    Statement** statements; // darray
} Codeblock;

typedef struct WhileStatement {
    Statement base;
    Expression* condition;
    Codeblock scope;
} WhileStatement;

typedef struct IfStatement {
    Statement base;
    Expression* condition; // if condition is null, this is an else-statement
    Codeblock scope;
    struct IfStatement* next;
} IfStatement;

typedef struct ReturnStatement {
    Statement base;
    Expression* returnExpr;
} ReturnStatement;

// ----Functions----------------------------------------------

typedef struct FuncArg {
    PlangType type;
    StrSpan name;
} FuncArg;

typedef struct FuncDeclaration {
    StrSpan name;
    PlangType returnType;
    FuncArg* arguments; // darray
} FuncDeclaration;

typedef struct PlangFunction {
    FuncDeclaration decl;
    Codeblock scope;
} PlangFunction;

typedef struct FuncCall {
    FuncDeclaration* function;
    ValuePath* valuePath;
    Expression** args; // darray of Expression pointers
} FuncCall;

// typedef struct FuncCallExpression {
//     FuncCall call;
// } FuncCallExpression;

typedef struct FuncCallStatement {
    Statement base;
    FuncCall call;
} FuncCallStatement;

// ----Struct----------------------------------------------

typedef struct Field {
    PlangType type;
    StrSpan name;
} Field;

typedef struct PlangStruct {
    StrSpan name;
    Field* fields; // darray
} PlangStruct;


extern PlangFunction* functions;
extern FuncDeclaration* functionDeclarations;
extern PlangStruct* structs;
extern VarDecl* globalVariables;