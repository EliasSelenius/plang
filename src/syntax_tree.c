

#define for_each_binary_node(X, A)\
    X(Plus, A) X(Minus, A) X(Mul, A) X(Div, A) X(Mod, A)\
    X(Less, A) X(Greater, A) X(LessEquals, A) X(GreaterEquals, A)\
    X(Equals, A) X(NotEquals, A)\
    X(BooleanAnd, A) X(BooleanOr, A)\
    X(Bitwise_And, A) X(Bitwise_Or, A) X(Bitwise_Xor, A)\
    X(Bitwise_Lshift, A) X(Bitwise_Rshift, A)\

#define for_each_unary_node(X, A)\
    X(Unary_PreIncrement, A) X(Unary_PostIncrement, A)\
    X(Unary_PreDecrement, A) X(Unary_PostDecrement, A)\
    X(Unary_Not, A) X(Unary_BitwiseNot, A)\
    X(Unary_AddressOf, A) X(Unary_ValueOf, A)\
    X(Unary_Negate, A)\

#define for_each_literal_node(X, A)\
    X(Literal_Integer, A) X(Literal_Decimal, A)\
    X(Literal_Char, A) X(Literal_String, A)\
    X(Literal_True, A) X(Literal_False, A) X(Literal_Null, A)\

#define for_each_other_expr_node(X, A)\
    X(Variable, A) X(Alloc, A) X(Ternary, A) X(ProcCall, A) X(Deref, A)\
    X(Indexing, A) X(Cast, A) X(Sizeof, A) X(Parenthesized, A) X(Compound, A)\

#define for_each_stmt_node(X, A)\
    X(Declaration, A) X(Constant, A) X(Typedef, A) X(Procedure, A) X(Argument, A) X(Struct, A) X(Enum, A)\
    X(EnumEntry, A) X(Assignment, A) X(Expression, A) X(Scope, A) X(If, A) X(While, A) X(For, A) X(Switch, A)\
    X(Continue, A) X(Break, A) X(Return, A) X(Goto, A) X(Label, A) X(CaseLabel, A) X(DefaultLabel, A)\

#define for_each_type_node(X, A)\
    X(Type_MustInfer, A) X(Type_Basic, A) X(Type_Procedure, A)\
    X(Type_Array, A) X(Type_Fixed_Array, A) X(Type_Dynamic_Array, A)\

#define for_each_expr_node(X, A)\
    for_each_binary_node(X, A)\
    for_each_unary_node(X, A)\
    for_each_literal_node(X, A)\
    for_each_other_expr_node(X, A)\

#define for_each_node(X, A)\
    for_each_expr_node(X, A)\
    for_each_stmt_node(X, A)\
    for_each_type_node(X, A)\


#define node_enum_entry(name, a) Node_##name,
typedef enum Nodekind {
    Node_Invalid = 0,
    for_each_node(node_enum_entry,)
    Nodekind_Count
} Nodekind;
#undef node_enum_entry

#define for_each_node_struct(X)\
    for_each_stmt_node(X, Stmt)\
    for_each_other_expr_node(X, Expression)\
    X(Binary, Expression)\
    X(Unary, Expression)\
    X(Literal, Expression)\
    X(Type,)\

#define typedef_node(name, a) typedef struct name##a name##a;
for_each_node_struct(typedef_node)
#undef typedef_node


typedef struct AstNode {
    CodeLocation loc;
    Nodekind kind;
} AstNode;

#define AstNodePointer_field(name, a) name##a* name;
typedef union AstNodePointer {
    for_each_node_struct(AstNodePointer_field)
} AstNodePointer;
#undef AstNodePointer_field


// ----Types---------------------------------------------

typedef enum Typekind {

    Typekind_Invalid = 0, // used by validator to signify a type that could not be determined because of an error
    Typekind_AmbiguousInteger,
    Typekind_AmbiguousDecimal,

    Typekind_uint8,
    Typekind_uint16,
    Typekind_uint32,
    Typekind_uint64,

    Typekind_int8,
    Typekind_int16,
    Typekind_int32,
    Typekind_int64,

    Typekind_float32,
    Typekind_float64,

    Typekind_void,
    Typekind_char,

    Typekind_Struct,
    Typekind_Enum,
    Typekind_Typedef,
    Typekind_Procedure,

    Typekind_Array,
    Typekind_Fixed_Array,
    Typekind_Dynamic_Array
} Typekind;

/* assignability chart by Typekind

assignable? = yes | no | x
x = circumstantial

from \ to   Invalid AmbInteger  AmbDecimal  uint8   uint16  uint32  uint64  int8    int16   int32   int64   float32 float64 void    char    Struct  Enum    Typedef Opaque  Procedure
Invalid     yes     yes         yes         yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     yes
AmbInteger  yes     yes         yes         yes     yes     yes     yes     yes     yes     yes     yes     yes     yes     no      yes     no      no      x       no      no
AmbDecimal  yes     no          yes         no      no      no      no      no      no      no      no      yes     yes     no      no      no      no      x       no      no
uint8       yes     yes         no          yes     yes     yes     yes     no      yes     yes     yes     yes     yes     no      yes     no      no      x       no      no
uint16      yes     yes         no          no      yes     yes     yes     no      no      yes     yes     yes     yes     no      no      no      no      x       no      no
uint32      yes     yes         no          no      no      yes     yes     no      no      no      yes     yes     yes     no      no      no      no      x       no      no
uint64      yes     yes         no          no      no      no      yes     no      no      no      no      yes     yes     no      no      no      no      x       no      no
int8        yes     yes         no          no      no      no      no      yes     yes     yes     yes     yes     yes     no      yes     no      no      x       no      no
int16       yes     yes         no          no      no      no      no      no      yes     yes     yes     yes     yes     no      no      no      no      x       no      no
int32       yes     yes         no          no      no      no      no      no      no      yes     yes     yes     yes     no      no      no      no      x       no      no
int64       yes     yes         no          no      no      no      no      no      no      no      yes     yes     yes     no      no      no      no      x       no      no
float32     yes     no          yes         no      no      no      no      no      no      no      no      yes     yes     no      no      no      no      x       no      no
float64     yes     no          yes         no      no      no      no      no      no      no      no      no      yes     no      no      no      no      x       no      no
void        yes     no          no          no      no      no      no      no      no      no      no      no      no      yes     no      no      no      x       no      no
char        yes     yes         no          yes     yes     yes     yes     yes     yes     yes     yes     no      no      no      yes     no      no      x       no      no
Struct      yes     no          no          no      no      no      no      no      no      no      no      no      no      no      no      yes     no      x       no      no
Enum        yes     yes         no          yes     yes     yes     yes     yes     yes     yes     yes     no      no      no      no      no      yes     x       no      no
Typedef     yes
Opaque      yes
Procedure   yes


*/

typedef struct Datatype {
    Typekind kind;
    u32 numPointers;
    union {
        void* data_ptr;
        StructStmt* stru;
        EnumStmt* _enum;
        TypedefStmt* type_def;
        Type* proc_ptr_typenode;
        Type* array_typenode;
    };
} Datatype;


struct Type {
    AstNode nodebase;
    Datatype solvedstate;

    union {
        Identifier name;

        struct {
            struct Type* return_type;
            struct Type* first_argument;
        } procedure;

        struct {
            struct Type* element_type;
            struct Expression* size_expr;
        } array;
    };

    struct Type* next;
};


#define type_invalid          (Datatype) { .kind = Typekind_Invalid }
#define type_voidPointer      (Datatype) { .kind = Typekind_void, .numPointers = 1 }
#define type_charPointer      (Datatype) { .kind = Typekind_char, .numPointers = 1 }
#define type_ambiguousInteger (Datatype) { .kind = Typekind_AmbiguousInteger }
#define type_ambiguousDecimal (Datatype) { .kind = Typekind_AmbiguousDecimal }

#define type_int8    (Datatype) { .kind = Typekind_int8 }
#define type_uint8   (Datatype) { .kind = Typekind_uint8 }
#define type_int16   (Datatype) { .kind = Typekind_int16 }
#define type_uint16  (Datatype) { .kind = Typekind_uint16 }
#define type_int32   (Datatype) { .kind = Typekind_int32 }
#define type_uint32  (Datatype) { .kind = Typekind_uint32 }
#define type_int64   (Datatype) { .kind = Typekind_int64 }
#define type_uint64  (Datatype) { .kind = Typekind_uint64 }
#define type_float32 (Datatype) { .kind = Typekind_float32 }
#define type_float64 (Datatype) { .kind = Typekind_float64 }
#define type_char    (Datatype) { .kind = Typekind_char }
#define type_void    (Datatype) { .kind = Typekind_void }
#define type_bool    type_uint8

static inline bool typeEquals(Datatype a, Datatype b) {
    return a.kind == b.kind && a.data_ptr == b.data_ptr && a.numPointers == b.numPointers;
}

static inline Datatype resolveTypeAmbiguity(Datatype type) {
    if      (type.kind == Typekind_AmbiguousInteger) return type_int32;
    else if (type.kind == Typekind_AmbiguousDecimal) return type_float32;
    return type;
}

typedef struct { i32 min, max; } range;
static bool range_overlap(range a, range b) {
    return a.max >= b.min && a.min <= b.max;
}
static bool rangein(range this, range other) {
    return this.min >= other.min && this.max <= other.max;
}

static range getNumericDomain(Datatype type) {

    switch (type.kind) {
        case Typekind_uint8:  return (range){ 0, 2 };
        case Typekind_uint16: return (range){ 0, 4 };
        case Typekind_uint32: return (range){ 0, 8 };
        case Typekind_uint64: return (range){ 0, 16 };

        case Typekind_int8:  return (range){ -1, 1 };
        case Typekind_int16: return (range){ -2, 2 };
        case Typekind_int32: return (range){ -4, 4 };
        case Typekind_int64: return (range){ -8, 8 };

        default: break;
    }

    return (range){ 0, 0 };
}

static bool isIntegralType(Datatype type) {
    switch (type.kind) {
        case Typekind_uint8:  return true;
        case Typekind_uint16: return true;
        case Typekind_uint32: return true;
        case Typekind_uint64: return true;

        case Typekind_int8:  return true;
        case Typekind_int16: return true;
        case Typekind_int32: return true;
        case Typekind_int64: return true;

        default: break;
    }

    return false;
}

typedef struct NumberInfo {
    i32 bit_depth;
    bool is_signed;
    bool is_decimal;
} NumberInfo;

static NumberInfo getNumberInfo(Typekind type) {
    switch (type) {
        case Typekind_uint8:  return (NumberInfo) { .bit_depth = 8, .is_signed = false, .is_decimal = false };
        case Typekind_uint16: return (NumberInfo) { .bit_depth = 16, .is_signed = false, .is_decimal = false };
        case Typekind_uint32: return (NumberInfo) { .bit_depth = 32, .is_signed = false, .is_decimal = false };
        case Typekind_uint64: return (NumberInfo) { .bit_depth = 64, .is_signed = false, .is_decimal = false };
        case Typekind_int8:   return (NumberInfo) { .bit_depth = 8, .is_signed = true, .is_decimal = false };
        case Typekind_int16:  return (NumberInfo) { .bit_depth = 16, .is_signed = true, .is_decimal = false };
        case Typekind_int32:  return (NumberInfo) { .bit_depth = 32, .is_signed = true, .is_decimal = false };
        case Typekind_int64:  return (NumberInfo) { .bit_depth = 64, .is_signed = true, .is_decimal = false };

        case Typekind_float32: return (NumberInfo) { .bit_depth = 32, .is_signed = true, .is_decimal = true };
        case Typekind_float64: return (NumberInfo) { .bit_depth = 64, .is_signed = true, .is_decimal = true };

        case Typekind_AmbiguousInteger: return (NumberInfo) { .bit_depth = -1, .is_signed = false, .is_decimal = false };
        case Typekind_AmbiguousDecimal: return (NumberInfo) { .bit_depth = -1, .is_signed = true, .is_decimal = true };

        default: break;
    }

    return (NumberInfo) {0};
}

static NumberInfo mergeNumberInfos(NumberInfo a, NumberInfo b) {
    return (NumberInfo) {
        .bit_depth = a.bit_depth < b.bit_depth ? b.bit_depth : a.bit_depth,
        .is_signed = a.is_signed || b.is_signed,
        .is_decimal = a.is_decimal || b.is_decimal
    };
}

static Typekind getTypekindOfNumberInfo(NumberInfo info) {

    if (info.is_decimal) switch (info.bit_depth) {
        case -1: return Typekind_AmbiguousDecimal;
        case 32: return Typekind_float32;
        case 64: return Typekind_float64;
    }

    if (info.is_signed) switch (info.bit_depth) {
        case -1: return Typekind_AmbiguousInteger;
        case 8: return Typekind_int8;
        case 16: return Typekind_int16;
        case 32: return Typekind_int32;
        case 64: return Typekind_int64;
    } else switch (info.bit_depth) {
        case -1: return Typekind_AmbiguousInteger;
        case 8: return Typekind_uint8;
        case 16: return Typekind_uint16;
        case 32: return Typekind_uint32;
        case 64: return Typekind_uint64;
    }

    return Typekind_Invalid;
}

static Datatype mergeNumberTypes(Datatype a, Datatype b) {
    return (Datatype) { .kind = getTypekindOfNumberInfo(mergeNumberInfos(getNumberInfo(a.kind), getNumberInfo(b.kind))) };
}


// ----Expressions---------------------------------------------

typedef struct Expression {
    AstNode nodebase;
    Datatype datatype;
} Expression;

#define default_for_loop_numeric_type type_int32

typedef struct Statement {
    AstNode nodebase;
} Statement;

typedef struct CompoundElement {
    Expression* expr;
    Identifier name;
} CompoundElement;


#define DefineExpressionNode(name, stuff) struct name##Expression { Expression base; struct stuff;};
#define DefineStatementNode(name, stuff) struct name##Stmt { Statement base; struct stuff;};

DefineExpressionNode(Compound,      { CompoundElement* elements;                                                  })
DefineExpressionNode(Literal,       { Tokendata        data;                                                      })
DefineExpressionNode(Unary,         { Expression*      expr;                                                      })
DefineExpressionNode(Binary,        { Expression*      left;       Expression* right;                             })
DefineExpressionNode(Parenthesized, { Expression*      innerExpr;                                                 })
DefineExpressionNode(Indexing,      { Expression*      indexed;    Expression* index;                             })
DefineExpressionNode(Alloc,         { Expression*      sizeExpr;   Type*       type;                              })
DefineExpressionNode(Deref,         { Expression*      expr;       Identifier  name;                              })
DefineExpressionNode(Variable,      { Identifier       name;       Statement*  ref;                               }) // ref = procedure, global, enum
DefineExpressionNode(Cast,          { Expression*      expr;       Type*       castToType;                        })
DefineExpressionNode(Sizeof,        { Type*            type;                                                      })
DefineExpressionNode(Ternary,       { Expression*      condition;  Expression* thenExpr;    Expression* elseExpr; })

// ----Statements----------------------------------------------
DefineStatementNode(Typedef,     { Type*       type;         Identifier name;                                                       })
DefineStatementNode(Assignment,  { Expression* assigneeExpr; TokenType assignmentOper;  Expression* expr;                           })
DefineStatementNode(Scope,       { ScopeStmt*  parentScope;  Statement** statements; /* list */                                     })
DefineStatementNode(While,       { Expression* condition;    Statement* statement;                                                  })
DefineStatementNode(If,          { Expression* condition;    Statement* then_statement; Statement* else_statement;                  })
DefineStatementNode(Switch,      { Expression* expr;         ScopeStmt* scope;                                                      })
DefineStatementNode(Return,      { Expression* returnExpr;                                                                          })
DefineStatementNode(Goto,        { Identifier  label;                                                                               })
DefineStatementNode(Label,       { Identifier  label;                                                                               })
DefineStatementNode(CaseLabel,   { Expression* expr;         SwitchStmt* switch_statement;                                          })
DefineStatementNode(Argument,    { Type*       type;         Identifier name;                                                       })


DefineStatementNode(Declaration, {
    Type*       type;
    Expression* expr; /* can be null */
    Identifier name;
    bool include_context; /* TODO: flags */
    bool is_static;
})

DefineStatementNode(For, {
    Type*       index_type;   /* can be null */
    Identifier  index_name;
    Expression* iterator_assignment;
    union { Expression* min_expr; Expression* condition; };
    union { Expression* max_expr; Expression* iterator_update; };
    Statement* statement;
})

DefineStatementNode(Procedure, {
    Identifier name;
    Type* returnType;
    ArgumentStmt* arguments; // list, can be null

    /* overload
        value of zero means this function is not overloaded.
        a value of N means this function is the N'th function in the set of all its siblings. Where N > 0
    */
    u32 overload;
    ProcedureStmt* next_overload;

    ScopeStmt* scope; // scope can be null

    Type* type_node;
})

DefineExpressionNode(ProcCall, {
    Expression* proc_expr;
    Expression** args; // list, can be null
    ProcedureStmt* proc; // can be null if this ProcCall is calling a procptr
})


// typedef struct Field {
//     Type* type;
//     Identifier name;
//     Expression* expr;
//     u32 byte_offset;
//     bool include_context;
// } Field;

DefineStatementNode(Struct, {
    Identifier name;
    u32 byte_size;
    u32 deps; // TODO: temporary, until we figure out a better way of transpiling structs in the correct order
    DeclarationStmt* fields; // list
})

DefineStatementNode(EnumEntry, {
    EnumStmt* _enum;
    Expression* expr; // can be null
    u64 value;
    Identifier name;
})

DefineStatementNode(Enum, {
    Identifier name;
    EnumEntryStmt* entries; // list
})

static DeclarationStmt* getField(StructStmt* stru, Identifier name) {
    u32 len = list_length(stru->fields);
    for (u32 i = 0; i < len; i++)
        if (stru->fields[i].name == name) return &stru->fields[i];
    return null;
}

static Statement* getMember(StructStmt* stru, Identifier name) {
    foreach (field, stru->fields) {
        if (field->name == name) return (Statement*)field;

        if (field->include_context) {
            StructStmt* sub_stru = field->type->solvedstate.stru;
            Statement* sta = getMember(sub_stru, name);
            if (sta) return sta;
        }
    }
    return null;
}

static EnumEntryStmt* getEnumEntry(EnumStmt* en, Identifier name) {
    u32 len = list_length(en->entries);
    for (u32 i = 0; i < len; i++) if (en->entries[i].name == name) {
        return &en->entries[i];
    }
    return null;
}

static Datatype dealiasType(Datatype type) {
    if (type.kind == Typekind_Typedef) {
        if (type.type_def->type == null) return type; // opaque types
        Datatype newType = type.type_def->type->solvedstate;
        newType.numPointers += type.numPointers;
        return dealiasType(newType);
    }
    return type;
}

// #define index_initializer(name, value) [Node_##name] = value,
// #define index_initializer_binary(name)      index_initializer(name, sizeof(BinaryExpression))
// #define index_initializer_unary(name)       index_initializer(name, sizeof(UnaryExpression))
// #define index_initializer_literal(name)     index_initializer(name, sizeof(LiteralExpression))
// #define index_initializer_other_expr(name)  index_initializer(name, sizeof(name##Expression))
// #define index_initializer_stmt(name)        index_initializer(name, sizeof(name##Stmt))

// static u32 node_struct_sizes[] = {
//     for_each_binary_node(index_initializer_binary)
//     for_each_unary_node(index_initializer_unary)
//     for_each_literal_node(index_initializer_literal)
//     for_each_other_expr_node(index_initializer_other_expr)
//     for_each_stmt_node(index_initializer_stmt)
// };

// #undef index_initializer_binary
// #undef index_initializer_unary
// #undef index_initializer_literal
// #undef index_initializer_other_expr
// #undef index_initializer_stmt

// #define index_initializer_string(name) index_initializer(name, #name)
// static char* node_names[] = {
//     for_each_node(index_initializer_string)
// };
// #undef index_initializer_string
// #undef index_initializer

// static bool isCompiletimeExpression(Expression* expr) {
//     ExprPointer e = (ExprPointer)expr;
//     switch (expr->expressionType) {
//         case ExprType_Plus:
//         case ExprType_Minus:
//         case ExprType_Mul:
//         case ExprType_Div:
//         case ExprType_Mod:
//         case ExprType_Less:
//         case ExprType_Greater:
//         case ExprType_LessEquals:
//         case ExprType_GreaterEquals:
//         case ExprType_Equals:
//         case ExprType_NotEquals:
//         case ExprType_BooleanAnd:
//         case ExprType_BooleanOr:
//         case ExprType_Bitwise_And:
//         case ExprType_Bitwise_Or:
//         case ExprType_Bitwise_Xor:
//         case ExprType_Bitwise_Lshift:
//         case ExprType_Bitwise_Rshift:
//             return isCompiletimeExpression(e.binary->left) && isCompiletimeExpression(e.binary->right);

//         case ExprType_Unary_PreIncrement:
//         case ExprType_Unary_PostIncrement:
//         case ExprType_Unary_PreDecrement:
//         case ExprType_Unary_PostDecrement:
//             return false;

//         case ExprType_Unary_Not:
//         case ExprType_Unary_BitwiseNot:
//         case ExprType_Unary_Negate:
//         case ExprType_Unary_ValueOf:
//             return isCompiletimeExpression(e.unary->expr);

//         case ExprType_Unary_AddressOf: return false;

//         case ExprType_Literal_Integer:
//         case ExprType_Literal_Decimal:
//         case ExprType_Literal_Char:
//         case ExprType_Literal_String:
//         case ExprType_Literal_True:
//         case ExprType_Literal_False:
//         case ExprType_Literal_Null:
//             return true;

//         case ExprType_Variable: return e.var->ref->statementType == Statement_Constant;
//         case ExprType_Alloc: return false;
//         case ExprType_Ternary: return isCompiletimeExpression(e.ternary->condition) && isCompiletimeExpression(e.ternary->thenExpr) && isCompiletimeExpression(e.ternary->elseExpr);
//         case ExprType_ProcCall: return false;

//         case ExprType_Deref:
//             if (e.deref->base.datatype.kind == Typekind_Enum) return true;
//             return isCompiletimeExpression(e.deref->expr);

//         case ExprType_Indexing: return isCompiletimeExpression(e.indexing->indexed) && isCompiletimeExpression(e.indexing->index);
//         case ExprType_Cast: return isCompiletimeExpression(e.cast->expr);
//         case ExprType_Sizeof: return true;
//         case ExprType_Parenthesized: return isCompiletimeExpression(e.parenth->innerExpr);

//         case ExprType_Compound: {
//             if (!e.compound->elements) return true;
//             foreach (elm, e.compound->elements) {
//                 if (!isCompiletimeExpression(elm->expr)) return false;
//             }

//             return true;
//         }
//     }
// }
