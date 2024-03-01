

typedef struct Procedure Procedure;
typedef struct ForStatement ForStatement;
typedef struct Struct Struct;
typedef struct Enum Enum;
typedef struct Typedef Typedef;
typedef struct Datatype Datatype;
typedef struct ProcArg ProcArg;
typedef struct Declaration Declaration;
typedef struct Statement Statement;
typedef struct VariableExpression VariableExpression;
typedef struct Type Type;

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



typedef struct Node {
    CodeLocation loc;
} Node;

typedef enum TypeNode {
    TypeNode_MustInfer,
    TypeNode_Normal,
    TypeNode_Procedure,
    TypeNode_Array,
    TypeNode_Fixed_Array,
    TypeNode_Dynamic_Array
} TypeNode;

typedef struct Type {
    Node nodebase;
    TypeNode node_type;

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
} Type;


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

// TODO:
// typedef enum BinaryOperator {
//     BinaryOperator_Addition,
//     BinaryOperator_Subtraction,
//     BinaryOperator_Multiplication,
//     BinaryOperator_Division,
//     BinaryOperator_Modulus
// } BinaryOperator;

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
    ExprType_Alloc,
    ExprType_Ternary,
    ExprType_ProcCall,
    ExprType_Deref,
    ExprType_Indexing,
    ExprType_Cast,
    ExprType_Sizeof,
    ExprType_Parenthesized,
    ExprType_Compound

} ExprType;

typedef struct Expression {
    Node nodebase;
    ExprType expressionType;
    Datatype datatype;
} Expression;

typedef struct CompoundElement {
    Expression* expr;
    Identifier name;
} CompoundElement;

typedef struct CompoundExpression {
    Expression base;
    CompoundElement* elements; // list, can be null
} CompoundExpression;

typedef struct LiteralExpression {
    Expression base;
    Tokendata data;
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
    Type* type;
} AllocExpression;

typedef struct DerefOperator {
    Expression base;
    Expression* expr;
    Identifier name;
} DerefOperator;

typedef struct VariableExpression {
    Expression base;
    Identifier name;

    Statement* ref; // procedure, global, enum
} VariableExpression;

typedef struct CastExpression {
    Expression base;
    Expression* expr;
    Type* castToType;
} CastExpression;

typedef struct SizeofExpression {
    Expression base;
    Type* type;
} SizeofExpression;

typedef struct TernaryExpression {
    Expression base;
    Expression* condition;
    Expression* thenExpr;
    Expression* elseExpr;
} TernaryExpression;


// ----Statements----------------------------------------------

typedef enum StatementType {
    Statement_Declaration,
    Statement_Constant,
    Statement_Typedef,
    Statement_Procedure,
    Statement_Argument,
    Statement_Struct,
    Statement_Enum,
    Statement_EnumEntry,

    Statement_Assignment,
    Statement_Expression,

    Statement_Scope,
    Statement_If,
    Statement_While,
    Statement_For,
    Statement_Switch,

    Statement_Continue,
    Statement_Break,
    Statement_Return,
    Statement_Goto,
    Statement_Label,
    Statement_CaseLabel,
    Statement_DefaultLabel
} StatementType;

typedef struct Statement {
    Node nodebase;
    StatementType statementType;
} Statement;

typedef struct StatementExpression {
    Statement base;
    Expression* expr;
} StatementExpression;

typedef struct Declaration {
    Statement base;
    Type* type;
    Expression* expr; // can be null
    Identifier name;
    bool include_context; // TODO: flags
    bool is_static;
} Declaration;

typedef struct Typedef {
    Statement base;
    Type* type;
    Identifier name;
} Typedef;

typedef struct Assignment {
    Statement base;
    Expression* assigneeExpr;
    TokenType assignmentOper; // = += -= *= /= etc...
    Expression* expr;
} Assignment;

typedef struct Scope {
    Statement base;
    struct Scope* parentScope;
    Statement** statements; // list
} Scope;

typedef struct WhileStatement {
    Statement base;
    Expression* condition;
    Statement* statement;
} WhileStatement;

#define default_for_loop_numeric_type type_int32
typedef struct ForStatement {
    Statement base;
    Type* index_type; // can be null
    Identifier index_name;
    Expression* iterator_assignment;
    union {
        Expression* min_expr;
        Expression* condition;
    };
    union {
        Expression* max_expr;
        Expression* iterator_update;
    };
    Statement* statement;
} ForStatement;

typedef struct IfStatement {
    Statement base;
    Expression* condition;
    Statement* then_statement;
    Statement* else_statement;
} IfStatement;

typedef struct SwitchStatement {
    Statement base;
    Expression* expr;
    Scope* scope;
} SwitchStatement;

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

typedef struct CaseLabelStatement {
    Statement base;
    Expression* expr;
    SwitchStatement* switch_statement;
} CaseLabelStatement;


// ----Procedures----------------------------------------------

typedef struct ProcArg {
    Statement base;
    Type* type;
    Identifier name;
} ProcArg;

typedef struct Procedure {
    Statement base;
    Identifier name;
    Type* returnType;
    ProcArg* arguments; // list, can be null

    /* overload
        value of zero means this function is not overloaded.
        a value of N means this function is the N'th function in the set of all its siblings. Where N > 0
    */
    u32 overload;
    struct Procedure* next_overload;

    Scope* scope; // scope can be null

    Type* type_node;
} Procedure;

typedef struct ProcCall {
    Expression base;
    Expression* proc_expr;
    Expression** args; // list, can be null
    Procedure* proc; // can be null if this ProcCall is calling a procptr
} ProcCall;

typedef struct CapturedVariable {
    Identifier name;
    Datatype type;
} CapturedVariable;

// ----------------------------------------------------------

typedef union ExprPointer {
    Expression* expr;
    VariableExpression* var;
    DerefOperator* deref;
    TernaryExpression* ternary;
    BinaryExpression* binary;
    UnaryExpression* unary;
    LiteralExpression* literal;
    ParenthesizedExpression* parenth;
    CastExpression* cast;
    SizeofExpression* size_of;
    AllocExpression* alloc;
    CompoundExpression* compound;
    IndexingExpression* indexing;
    ProcCall* call;
} ExprPointer;

typedef union StmtPointer {
    Statement* sta;
    StatementExpression* expr;
    Declaration* decl;
    Typedef* type_def;
    Assignment* assign;
    Scope* scope;
    WhileStatement* while_loop;
    ForStatement* for_loop;
    IfStatement* if_sta;
    SwitchStatement* switch_sta;
    ReturnStatement* ret_sta;
    GotoStatement* goto_sta;
    LabelStatement* label;
    CaseLabelStatement* case_sta;
} StmtPointer;

// ----Struct----------------------------------------------

// typedef struct Field {
//     Type* type;
//     Identifier name;
//     Expression* expr;
//     u32 byte_offset;
//     bool include_context;
// } Field;

typedef struct Struct {
    Statement base;
    Identifier name;
    u32 byte_size;
    u32 deps; // TODO: temporary, until we figure out a better way of transpiling structs in the correct order
    Declaration* fields; // list
} Struct;

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

typedef struct EnumEntry {
    Statement base;
    Enum* _enum;
    Expression* expr; // can be null
    u64 value;
    Identifier name;
} EnumEntry;

typedef struct Enum {
    Statement base;
    Identifier name;
    EnumEntry* entries; // list
} Enum;

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

static bool isCompiletimeExpression(Expression* expr) {
    ExprPointer e = (ExprPointer)expr;
    switch (expr->expressionType) {
        case ExprType_Plus:
        case ExprType_Minus:
        case ExprType_Mul:
        case ExprType_Div:
        case ExprType_Mod:
        case ExprType_Less:
        case ExprType_Greater:
        case ExprType_LessEquals:
        case ExprType_GreaterEquals:
        case ExprType_Equals:
        case ExprType_NotEquals:
        case ExprType_BooleanAnd:
        case ExprType_BooleanOr:
        case ExprType_Bitwise_And:
        case ExprType_Bitwise_Or:
        case ExprType_Bitwise_Xor:
        case ExprType_Bitwise_Lshift:
        case ExprType_Bitwise_Rshift:
            return isCompiletimeExpression(e.binary->left) && isCompiletimeExpression(e.binary->right);

        case ExprType_Unary_PreIncrement:
        case ExprType_Unary_PostIncrement:
        case ExprType_Unary_PreDecrement:
        case ExprType_Unary_PostDecrement:
            return false;

        case ExprType_Unary_Not:
        case ExprType_Unary_BitwiseNot:
        case ExprType_Unary_Negate:
        case ExprType_Unary_ValueOf:
            return isCompiletimeExpression(e.unary->expr);

        case ExprType_Unary_AddressOf: return false;

        case ExprType_Literal_Integer:
        case ExprType_Literal_Decimal:
        case ExprType_Literal_Char:
        case ExprType_Literal_String:
        case ExprType_Literal_True:
        case ExprType_Literal_False:
        case ExprType_Literal_Null:
            return true;

        case ExprType_Variable: return e.var->ref->statementType == Statement_Constant;
        case ExprType_Alloc: return false;
        case ExprType_Ternary: return isCompiletimeExpression(e.ternary->condition) && isCompiletimeExpression(e.ternary->thenExpr) && isCompiletimeExpression(e.ternary->elseExpr);
        case ExprType_ProcCall: return false;

        case ExprType_Deref:
            if (e.deref->base.datatype.kind == Typekind_Enum) return true;
            return isCompiletimeExpression(e.deref->expr);

        case ExprType_Indexing: return isCompiletimeExpression(e.indexing->indexed) && isCompiletimeExpression(e.indexing->index);
        case ExprType_Cast: return isCompiletimeExpression(e.cast->expr);
        case ExprType_Sizeof: return true;
        case ExprType_Parenthesized: return isCompiletimeExpression(e.parenth->innerExpr);

        case ExprType_Compound: {
            if (!e.compound->elements) return true;
            foreach (elm, e.compound->elements) {
                if (!isCompiletimeExpression(elm->expr)) return false;
            }

            return true;
        }
    }
}
