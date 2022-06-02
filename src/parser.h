#pragma once

#include "types.h"
#include "essh-string.h"
#include "lexer.h"
#include "darray.h"
#include "dynamic_buffer.h"

u32 parse();

typedef struct PlangFunction PlangFunction;
typedef struct FuncDeclaration FuncDeclaration;
typedef struct PlangStruct PlangStruct;
typedef struct VarDecl VarDecl;
typedef struct PlangType PlangType;

typedef struct TranslationUnit {
    PlangFunction* functions; // darray
    FuncDeclaration* functionDeclarations; // darray
    PlangStruct* structs; // darray
    VarDecl* globalVariables; // darray

    // darray of all ocuring types, no duplicates
    PlangType* types;
    DynamicBuffer* funcPtrTypes;

} TranslationUnit;

extern TranslationUnit* g_Unit;

typedef struct Node {
    u32 lineNumber;
} Node;

typedef enum Typekind {
    Typekind_Invalid = 0,
    Typekind_Primitive,
    Typekind_Struct,
    Typekind_Enum,
    Typekind_FuncPtr
} Typekind;

typedef struct PlangType {
    Typekind kind;
    StrSpan name;
    union {
        PlangStruct* type_struct;
        u32          type_funcPtr;
    };
} PlangType;

typedef struct Datatype {
    u32 typeId;
    u32 numPointers;
} Datatype;

typedef struct FuncPtr {
    Datatype returnType;
    u32 argCount;
    Datatype argTypes[];
} FuncPtr;

#define type_null ((Datatype){0})


inline PlangType* getType(Datatype dt) {
    return &g_Unit->types[dt.typeId - 1];
}

inline bool typeMustBeInfered(Datatype dt) {
    return dt.typeId == 0;
}

inline u32 ensureTypeExistence(StrSpan name) {
    u32 len = darrayLength(g_Unit->types);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(g_Unit->types[i].name, name)) {
            return i + 1;
        }
    }

    PlangType newType;
    newType.name = name;
    newType.kind = Typekind_Invalid;
    darrayAdd(g_Unit->types, newType);
    return len + 1;
}

inline FuncPtr* getFuncPtr(u32 id) { return (FuncPtr*)&g_Unit->funcPtrTypes->bytes[id]; }



// ----Expressions---------------------------------------------


typedef enum ExprType {
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
    ExprType_BooleanAnd = Tok_Keyword_And,
    ExprType_BooleanOr = Tok_Keyword_Or,

    ExprType_Unary_Not,
    ExprType_Unary_AddressOf,
    ExprType_Unary_ValueOf,

    ExprType_Literal_Integer,
    ExprType_Literal_Decimal,
    ExprType_Literal_String,
    ExprType_Literal_Bool,
    ExprType_Literal_Null,
    ExprType_Variable,
    ExprType_Alloc,
    ExprType_Ternary,
    ExprType_FuncCall,
    ExprType_FuncPointerCall,
    ExprType_Deref,

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

typedef struct UnaryExpression {
    Expression base;
    Expression* expr;
} UnaryExpression;

typedef struct BinaryExpression {
    Expression base;
    Expression* left;
    Expression* right;
} BinaryExpression;

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
    Datatype type;
    StrSpan name;
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
    Datatype type;
    StrSpan name;
} FuncArg;

typedef struct FuncDeclaration {
    StrSpan name;
    Datatype returnType;
    FuncArg* arguments; // darray
} FuncDeclaration;

typedef struct PlangFunction {
    FuncDeclaration decl;
    Codeblock scope;
} PlangFunction;

typedef struct FuncCall {
    Expression base;
    // FuncDeclaration* function;
    StrSpan functionName;
    Expression* funcExpr;
    Expression** args; // darray of Expression pointers
} FuncCall;

// ----Struct----------------------------------------------

typedef struct Field {
    Node nodebase;
    Datatype type;
    StrSpan name;
} Field;

typedef struct PlangStruct {
    Node nodebase;
    StrSpan name;
    Field* fields; // darray
} PlangStruct;
