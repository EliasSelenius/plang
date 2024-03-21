

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
    X(EnumEntry, A) X(Assignment, A) X(Scope, A) X(IfStmt, A) X(WhileStmt, A) X(ForStmt, A) X(SwitchStmt, A)\
    X(ContinueStmt, A) X(BreakStmt, A) X(ReturnStmt, A) X(GotoStmt, A) X(LabelStmt, A) X(CaseLabelStmt, A) X(DefaultLabelStmt, A)\

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

#define index_initializer_string(name, value) [Node_##name] = #name,
static char* node_names[] = { for_each_node(index_initializer_string, ) };
#undef index_initializer_string

#define node_cases(name, a) case Node_##name:

#define for_each_node_struct(X)\
    for_each_stmt_node(X,)\
    for_each_other_expr_node(X, Expression)\
    X(Binary, Expression)\
    X(Unary, Expression)\
    X(Literal, Expression)\
    X(Type,)\

#define typedef_node(name, a) typedef struct name##a name##a;
for_each_node_struct(typedef_node)
#undef typedef_node

#define AstNodeFields struct { CodeLocation loc; Nodekind kind; };
typedef struct AstNode { AstNodeFields } AstNode;


#define AstNodePointer_field(name, a) name##a* name;
typedef union NodeRef {
    void* void_ptr;
    AstNode* node;
    struct Expression* expr;
    struct Statement* stmt;
    for_each_node_struct(AstNodePointer_field)
} NodeRef;
#undef AstNodePointer_field

#define null_node ((NodeRef){null})


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
        Struct* stru;
        Enum* _enum;
        Typedef* type_def;
        Type* proc_ptr_typenode;
        Type* array_typenode;
    };
} Datatype;


struct Type {
    AstNodeFields
    Datatype solvedstate;

    union {
        Identifier name;

        struct {
            struct Type* return_type;
            struct Type* first_argument;
        } procedure;

        struct {
            struct Type* element_type;
            NodeRef size_expr;
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
    union { AstNode node; AstNodeFields };
    Datatype datatype;
} Expression;

typedef struct Statement {
    union { AstNode node; AstNodeFields };
} Statement;

#define default_for_loop_numeric_type type_int32

typedef struct CompoundElement {
    NodeRef expr;
    Identifier name;
} CompoundElement;


#define DefineExpressionNode(name, stuff) struct name##Expression { AstNodeFields Datatype datatype; struct stuff;};
#define DefineStatementNode(name, stuff)  struct name             { AstNodeFields struct stuff;};

DefineExpressionNode(Compound,      { CompoundElement* elements;                                                     })
DefineExpressionNode(Literal,       { Tokendata        data;                                                         })
DefineExpressionNode(Unary,         { NodeRef          inner_expr;                                                   })
DefineExpressionNode(Binary,        { NodeRef          left;       NodeRef     right;                                })
DefineExpressionNode(Parenthesized, { NodeRef          inner_expr;                                                   })
DefineExpressionNode(Indexing,      { NodeRef          indexed;    NodeRef     index;                                })
DefineExpressionNode(Alloc,         { NodeRef          size_expr;  Type*       type;                                 })
DefineExpressionNode(Deref,         { NodeRef          expr;       Identifier  name;                                 })
DefineExpressionNode(Variable,      { Identifier       name;       NodeRef     ref;                                  }) // ref = procedure, global, enum
DefineExpressionNode(Cast,          { NodeRef          expr;       Type*       new_type;                             })
DefineExpressionNode(Sizeof,        { Type*            type;                                                         })
DefineExpressionNode(Ternary,       { NodeRef          condition;  NodeRef     then_expr;   NodeRef    else_expr;    })
DefineExpressionNode(ProcCall,      { NodeRef          proc_expr;  NodeRef*    args;        Procedure* proc;         }) // args = list, can be null. proc = can be null if this ProcCall is calling a procptr

// ----Statements----------------------------------------------
DefineStatementNode(Typedef,        { Type*       type;         Identifier name;                                     })
DefineStatementNode(Assignment,     { NodeRef     dst_expr;     NodeRef    src_expr;        TokenType  operator;     })
DefineStatementNode(Scope,          { Scope*      parentScope;  NodeRef*   statements;                               })
DefineStatementNode(WhileStmt,      { NodeRef     condition;    NodeRef    statement;                                })
DefineStatementNode(IfStmt,         { NodeRef     condition;    NodeRef    then_statement;  NodeRef else_statement;  })
DefineStatementNode(SwitchStmt,     { NodeRef     expr;         Scope*     scope;                                    })
DefineStatementNode(ReturnStmt,     { NodeRef     expr;                                                              })
DefineStatementNode(GotoStmt,       { Identifier  label;                                                             })
DefineStatementNode(LabelStmt,      { Identifier  label;                                                             })
DefineStatementNode(CaseLabelStmt,  { NodeRef     expr;         SwitchStmt* switch_statement;                        })
DefineStatementNode(Argument,       { Type*       type;         Identifier  name;                                    })
DefineStatementNode(Constant,       { NodeRef     expr;         Identifier  name;                                    })
DefineStatementNode(DefaultLabelStmt, {})
DefineStatementNode(ContinueStmt,   {})
DefineStatementNode(BreakStmt,      {})



DefineStatementNode(Declaration, {
    Type*   type;
    NodeRef expr; /* can be null */
    Identifier name;
    bool include_context; /* TODO: flags */
    bool is_static;
})

DefineStatementNode(ForStmt, {
    Type*       index_type;   /* can be null */
    Identifier  index_name;
    NodeRef iterator_assignment;
    union { NodeRef min_expr; NodeRef condition; };
    union { NodeRef max_expr; NodeRef iterator_update; };
    NodeRef statement;
})

DefineStatementNode(Procedure, {
    Identifier name;
    Type* return_type;
    Argument* arguments; // list, can be null

    /* overload
        value of zero means this function is not overloaded.
        a value of N means this function is the N'th function in the set of all its siblings. Where N > 0
    */
    u32 overload;
    Procedure* next_overload;

    Scope* scope; // scope can be null

    Type* type_node;
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
    Declaration* fields; // list
})

DefineStatementNode(EnumEntry, {
    Enum* _enum;
    NodeRef expr; // can be null
    u64 value;
    Identifier name;
})

DefineStatementNode(Enum, {
    Identifier name;
    EnumEntry* entries; // list
})

static Declaration* getField(Struct* stru, Identifier name) {
    u32 len = list_length(stru->fields);
    for (u32 i = 0; i < len; i++)
        if (stru->fields[i].name == name) return &stru->fields[i];
    return null;
}

static Statement* getMember(Struct* stru, Identifier name) {
    foreach (field, stru->fields) {
        if (field->name == name) return (Statement*)field;

        if (field->include_context) {
            Struct* sub_stru = field->type->solvedstate.stru;
            Statement* sta = getMember(sub_stru, name);
            if (sta) return sta;
        }
    }
    return null;
}

static EnumEntry* getEnumEntry(Enum* en, Identifier name) {
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


#define index_initializer(name, value) [Node_##name] = sizeof(name##value),
#define index_initializer_const(name, value) [Node_##name] = sizeof(value),
static u32 node_struct_sizes[] = {
    for_each_binary_node      (index_initializer_const, BinaryExpression)
    for_each_unary_node       (index_initializer_const, UnaryExpression)
    for_each_literal_node     (index_initializer_const, LiteralExpression)
    for_each_other_expr_node  (index_initializer, Expression)
    for_each_stmt_node        (index_initializer, )
};
#undef index_initializer
#undef index_initializer_const



static bool isCompiletimeExpression(NodeRef e) {
    switch (e.node->kind) {
        default: return false;

        for_each_binary_node(node_cases,) return isCompiletimeExpression(e.Binary->left) && isCompiletimeExpression(e.Binary->right);

        case Node_Unary_PreIncrement:
        case Node_Unary_PostIncrement:
        case Node_Unary_PreDecrement:
        case Node_Unary_PostDecrement:
            return false;

        case Node_Unary_Not:
        case Node_Unary_BitwiseNot:
        case Node_Unary_Negate:
        case Node_Unary_ValueOf:
            return isCompiletimeExpression(e.Unary->inner_expr);

        case Node_Unary_AddressOf: return false;

        for_each_literal_node(node_cases,) return true;

        case Node_Variable: return e.Variable->ref.node->kind == Node_Constant;
        case Node_Alloc: return false;
        case Node_Ternary: return isCompiletimeExpression(e.Ternary->condition) && isCompiletimeExpression(e.Ternary->then_expr) && isCompiletimeExpression(e.Ternary->else_expr);
        case Node_ProcCall: return false; // TODO: if it is calling a pure function with comptime constant arguments

        case Node_Deref:
            if (e.Deref->datatype.kind == Typekind_Enum) return true;
            return isCompiletimeExpression(e.Deref->expr);

        case Node_Indexing: return isCompiletimeExpression(e.Indexing->indexed) && isCompiletimeExpression(e.Indexing->index);
        case Node_Cast: return isCompiletimeExpression(e.Cast->expr);
        case Node_Sizeof: return true;
        case Node_Parenthesized: return isCompiletimeExpression(e.Parenthesized->inner_expr);

        case Node_Compound: {
            if (!e.Compound->elements) return true;
            foreach (elm, e.Compound->elements) if (!isCompiletimeExpression(elm->expr)) return false;
            return true;
        }
    }
}
