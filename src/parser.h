#pragma once

#include "types.h"
#include "essh-string.h"
#include "lexer.h"
#include "darray.h"
#include "dynamic_buffer.h"
#include "typesystem.h"

u32 parse();

typedef struct PlangFunction PlangFunction;
typedef struct FuncDeclaration FuncDeclaration;
typedef struct PlangStruct PlangStruct;
typedef struct VarDecl VarDecl;
typedef struct Constant Constant;

typedef u32 Identifier;

typedef struct TranslationUnit {
    PlangFunction* functions; // darray
    FuncDeclaration* functionDeclarations; // darray
    PlangStruct* structs; // darray
    VarDecl* globalVariables; // darray
    Constant* constants; // darray

    DynamicBuffer* funcPtrTypes;

    AliasType* aliases; // darray
    Identifier* opaqueTypes; // darray

    u32* stringTableByteOffsets; // darray
    DynamicBuffer* stringTable;

} TranslationUnit;

extern TranslationUnit* g_Unit;

inline char* getIdentifierStringValue(Identifier id) {
    return (char*)(&g_Unit->stringTable->bytes[id]);
}

u32 appendStringToStringtable(StrSpan word);

typedef struct Node {
    u32 lineNumber;
    char* filepath;
} Node;

typedef struct FuncPtr {
    Datatype returnType;
    u32 argCount;
    Datatype argTypes[];
} FuncPtr;


inline FuncPtr* getFuncPtr(u32 id) { return (FuncPtr*)&g_Unit->funcPtrTypes->bytes[id]; }
Datatype ensureFuncPtrExistsFromFuncDeclaration(FuncDeclaration* decl);


// ----Expressions---------------------------------------------

typedef enum ExprType {

    ExprType_Plus = 1,
    ExprType_Minus,
    ExprType_Mul,
    ExprType_Div,
    ExprType_Mod,

    ExprType_Less,
    ExprType_Greater,
    ExprType_LessEquals,
    ExprType_GreaterEquals,
    ExprType_Equals,
    ExprType_NotEquals,
    ExprType_BooleanAnd,
    ExprType_BooleanOr,

    ExprType_Bitwise_And,
    ExprType_Bitwise_Or,
    ExprType_Bitwise_Xor,
    ExprType_Bitwise_Lshift,
    ExprType_Bitwise_Rshift,

    ExprType_Unary_PreIncrement,
    ExprType_Unary_PostIncrement,
    ExprType_Unary_PreDecrement,
    ExprType_Unary_PostDecrement,
    ExprType_Unary_Not,
    ExprType_Unary_BitwiseNot,
    ExprType_Unary_AddressOf,
    ExprType_Unary_ValueOf,
    ExprType_Unary_Negate,

    ExprType_Literal_Integer,
    ExprType_Literal_Decimal,

    ExprType_Literal_Char,
    ExprType_Literal_String,
    ExprType_Literal_True,
    ExprType_Literal_False,
    ExprType_Literal_Null,
    ExprType_Variable,
    ExprType_Constant, // TODO: is this even used for anything important?
    ExprType_Alloc,
    ExprType_Ternary,
    ExprType_FuncCall,
    ExprType_FuncPointerCall,
    ExprType_Deref,
    ExprType_Indexing,
    ExprType_Cast,
    ExprType_Sizeof,

    ExprType_Parenthesized

} ExprType;

typedef struct Expression {
    Node nodebase;
    ExprType expressionType;
} Expression;

typedef struct LiteralExpression {
    Expression base;
    union {
        Identifier string;
        u64 integer;
        f64 decimal;
        char character;
    };
} LiteralExpression;

typedef struct UnaryExpression {
    Expression base;
    Expression* expr;
} UnaryExpression;

typedef struct BinaryExpression {
    Expression base;
    Expression* left;
    Expression* right;
} BinaryExpression;

typedef struct ParenthesizedExpression {
    Expression base;
    Expression* innerExpr;
} ParenthesizedExpression;

typedef struct IndexingExpression {
    Expression base;
    Expression* indexed;
    Expression* index;
} IndexingExpression;

typedef struct AllocExpression {
    Expression base;
    Expression* sizeExpr;
    Datatype type;
} AllocExpression;

typedef struct DerefOperator {
    Expression base;
    Expression* expr;
    // I feel this is a little bit of a hack. We may have to remove this
    char* derefOp;
    Identifier name;
} DerefOperator;

typedef struct VariableExpression {
    Expression base;
    union {
        Identifier name;
        Expression* constExpr;
    };
} VariableExpression;

typedef struct CastExpression {
    Expression base;
    Expression* expr;
    Datatype castToType;
} CastExpression;

typedef struct SizeofExpression {
    Expression base;
    Datatype type;
} SizeofExpression;

typedef struct TernaryExpression {
    Expression base;
    Expression* condition;
    Expression* thenExpr;
    Expression* elseExpr;
} TernaryExpression;


u32 operatorPriority(ExprType type);

inline bool isBinaryExpression(Expression* expr) {
    return operatorPriority(expr->expressionType) != 0;
}


// ----Statements----------------------------------------------


typedef enum StatementType {
    Statement_Declaration,
    Statement_FixedArray_Declaration,
    Statement_Assignment,
    Statement_Expression,

    Statement_Scope,
    Statement_If,
    Statement_While,

    Statement_Continue,
    Statement_Break,
    Statement_Return,
    Statement_Goto,
    Statement_Label
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
    Datatype type;
    Identifier name;
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

typedef struct GotoStatement {
    Statement base;
    Identifier label;
} GotoStatement;

typedef struct LabelStatement {
    Statement base;
    Identifier label;
} LabelStatement;

// ----Functions----------------------------------------------

typedef struct FuncArg {
    Datatype type;
    Identifier name;
} FuncArg;

typedef struct FuncDeclaration {
    Identifier name;
    Datatype returnType;
    FuncArg* arguments; // darray
} FuncDeclaration;

typedef struct PlangFunction {
    FuncDeclaration decl;

    /* overload
        value of zero means this function is not overloaded.
        a value of N means this function is the N'th function in the set of all its siblings. Where N > 0
    */
    u32 overload;

    Codeblock scope;
} PlangFunction;

typedef struct FuncCall {
    Expression base;
    Expression* funcExpr;
    Expression** args; // darray of Expression pointers
    u32 overload;
} FuncCall;

// ----Struct----------------------------------------------

typedef struct Field {
    Node nodebase;
    Datatype type;
    Identifier name;
} Field;

typedef struct PlangStruct {
    Node nodebase;
    Identifier name;
    Field* fields; // darray
} PlangStruct;


typedef struct Constant {
    Identifier name;
    Expression* expr;
} Constant;
