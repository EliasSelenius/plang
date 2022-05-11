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
    ExprType_Literal_Number,
    ExprType_Literal_String,
    ExprType_Literal_Bool,
    ExprType_Literal_Null,
    ExprType_Variable,
    ExprType_Alloc,
    ExprType_Ternary,
    ExprType_FuncCall,


    ExprType_Plus = Tok_Plus,
    ExprType_Minus = Tok_Minus,
    ExprType_Mul = Tok_Mul,
    ExprType_Div = Tok_Div,
    
    ExprType_BooleanAnd,
    //ExprType_BooleanOr,
} ExprType;

typedef struct Expression {
    Node nodebase;
    ExprType expressionType;
} Expression;

// used by value path temporarily 
typedef struct ExpressionProxy {
    Expression base;
    void* node;
} ExpressionProxy;

typedef struct LiteralExpression {
    Expression base;
    StrSpan value; // TODO: maybe use different type than StrSpan
} LiteralExpression;

typedef struct BinaryExpression {
    Expression base;
    Expression* left;
    Expression* right;
} BinaryExpression;

typedef struct ArithmeticExpression {
    Expression base;
    u32 count;
    StrSpan* operators; // TODO: maybe use different type than StrSpan
    Expression** subExpressions;
} ArithmeticExpression;

typedef struct AllocExpression {
    Expression base;
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
    Expression base;
    Expression* condition;
    Expression* thenExpr;
    Expression* elseExpr;
} TernaryExpression;


// ----Statements----------------------------------------------


typedef enum StatementType {
    Statement_Declaration,
    Statement_Assignment,
    Statement_FuncCall,

    Statement_Scope,
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

typedef struct Scope {
    Statement base;
    Codeblock codeblock;
} Scope;

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
    bool mustInferReturnType;
} PlangFunction;

typedef struct FuncCall {
    FuncDeclaration* function;
    ValuePath* valuePath;
    Expression** args; // darray of Expression pointers
} FuncCall;

typedef struct FuncCallExpression {
    Expression base;
    FuncCall call;
} FuncCallExpression;

typedef struct FuncCallStatement {
    Statement base;
    FuncCall call;
} FuncCallStatement;

// ----Struct----------------------------------------------

typedef struct Field {
    Node nodebase;
    PlangType type;
    StrSpan name;
} Field;

typedef struct PlangStruct {
    Node nodebase;
    StrSpan name;
    Field* fields; // darray
} PlangStruct;


typedef struct TranslationUnit {
    PlangFunction* functions; // darray
    FuncDeclaration* functionDeclarations; // darray
    PlangStruct* structs; // darray
    VarDecl* globalVariables; // darray
} TranslationUnit;

extern PlangFunction* functions;
extern FuncDeclaration* functionDeclarations;
extern PlangStruct* structs;
extern VarDecl* globalVariables;