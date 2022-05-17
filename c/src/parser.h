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
    ExprType_Deref,


    ExprType_Plus = Tok_Plus,
    ExprType_Minus = Tok_Minus,
    ExprType_Mul = Tok_Mul,
    ExprType_Div = Tok_Div,

    ExprType_Less = Tok_LessThan,
    ExprType_Greater = Tok_GreaterThan,
    ExprType_LessEquals = Tok_LessThanOrEqual,
    ExprType_GreaterEquals = Tok_GreaterThanOrEqual,
    ExprType_Equals = Tok_Equals,
    ExprType_NotEquals = Tok_NotEquals,
    // NOTE: we might get into trouble if we add more ExprTypes 
    ExprType_BooleanAnd = Tok_Keyword_And,
    ExprType_BooleanOr = Tok_Keyword_Or,


} ExprType;

typedef struct Expression {
    Node nodebase;
    ExprType expressionType;
} Expression;

typedef struct LiteralExpression {
    Expression base;

    union {
        StrSpan value; // TODO: remove this
        char* string;
        u64 integer;
        f64 decimal;
    };

} LiteralExpression;

typedef struct BinaryExpression {
    Expression base;
    Expression* left;
    Expression* right;
} BinaryExpression;

typedef struct AllocExpression {
    Expression base;
    Expression* sizeExpr;
    PlangType type;
} AllocExpression;

typedef struct DerefOperator {
    Expression base;
    Expression* expr;
    StrSpan name;
} DerefOperator;

typedef struct VariableExpression {
    Expression base;
    StrSpan name;
} VariableExpression;

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
    Statement_Expression,

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

typedef struct StatementExpression {
    Statement base;
    Expression* expr;
} StatementExpression;

typedef struct VarDecl {
    Statement base;
    PlangType type;
    StrSpan name;
    bool mustInferType;
    Expression* assignmentOrNull;
} VarDecl;

typedef struct Assignement {
    Statement base;
    Expression* assigneeExpr;
    TokenType assignmentOper; // = += -= *= /=
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
    Expression base;
    FuncDeclaration* function;
    // ValuePath* valuePath;
    Expression* funcExpr;
    Expression** args; // darray of Expression pointers
} FuncCall;

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