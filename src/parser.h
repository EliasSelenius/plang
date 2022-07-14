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
typedef struct Constant Constant;
typedef struct PlangType PlangType;

typedef struct TranslationUnit {
    PlangFunction* functions; // darray
    FuncDeclaration* functionDeclarations; // darray
    PlangStruct* structs; // darray
    VarDecl* globalVariables; // darray
    Constant* constants; // darray

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
    Typekind_Alias,
    Typekind_FuncPtr
    // Typekind_FixedArray
} Typekind;

typedef struct Datatype {
    u32 typeId;
    u32 numPointers;
} Datatype;

typedef struct PlangType {
    Typekind kind;
    StrSpan name;
    union {
        Datatype     type_aliasedType;
        PlangStruct* type_struct;
        u32          type_funcPtr;
    };
} PlangType;

typedef struct FuncPtr {
    Datatype returnType;
    u32 argCount;
    Datatype argTypes[];
} FuncPtr;


#define type_null ((Datatype){0})

inline PlangType* getTypeById(u32 id) { return &g_Unit->types[id - 1]; }
inline PlangType* getType(Datatype dt) { return getTypeById(dt.typeId); }
inline bool typeExists(Datatype dt) { return dt.typeId != 0; }

inline PlangType* getActualType(Datatype dt) {
    PlangType* type = getType(dt);
    if (type->kind == Typekind_Alias) {
        if (type->type_aliasedType.typeId) type = getActualType(type->type_aliasedType);
    }
    return type;
}

inline bool typeEquals(Datatype a, Datatype b) {
    return a.typeId == b.typeId && a.numPointers == b.numPointers;
}


inline u32 ensureTypeExistence(StrSpan name) {
    u32 len = darrayLength(g_Unit->types);
    for (u32 i = 0; i < len; i++) {
        switch (g_Unit->types[i].kind) {
            case Typekind_Invalid:
            case Typekind_Alias:
            case Typekind_Primitive: {
                if (spanEqualsSpan(g_Unit->types[i].name, name)) {
                    return i + 1;
                }
            } break;

            default: break;
        }
    }

    PlangType newType;
    newType.name = name;
    newType.kind = Typekind_Invalid;
    darrayAdd(g_Unit->types, newType);
    return len + 1;
}

inline FuncPtr* getFuncPtr(u32 id) { return (FuncPtr*)&g_Unit->funcPtrTypes->bytes[id]; }
Datatype ensureFuncPtrExistsFromFuncDeclaration(FuncDeclaration* decl);


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

    ExprType_Unary_PreIncrement,
    ExprType_Unary_PostIncrement,
    ExprType_Unary_PreDecrement,
    ExprType_Unary_PostDecrement,
    ExprType_Unary_Not,
    ExprType_Unary_AddressOf,
    ExprType_Unary_ValueOf,

    ExprType_Literal_Integer,
    ExprType_Literal_Uint,
    ExprType_Literal_Long,
    ExprType_Literal_ULong,
    ExprType_Literal_Decimal,
    ExprType_Literal_Float,
    ExprType_Literal_Double,

    ExprType_Literal_Char,
    ExprType_Literal_String,
    ExprType_Literal_Bool,
    ExprType_Literal_Null,
    ExprType_Variable,
    ExprType_Constant,
    ExprType_Alloc,
    ExprType_Ternary,
    ExprType_FuncCall,
    ExprType_FuncPointerCall,
    ExprType_Deref,
    ExprType_Indexing,
    ExprType_Cast

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
    StrSpan name;
} DerefOperator;

typedef struct VariableExpression {
    Expression base;
    union {
        StrSpan name;
        Expression* constExpr;
    };
} VariableExpression;

typedef struct CastExpression {
    Expression base;
    Expression* expr;
    Datatype castToType;
} CastExpression;

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


typedef struct Constant {
    StrSpan name;
    Expression* expr;
} Constant;
