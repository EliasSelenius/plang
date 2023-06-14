

// signifies a string thats stored in the string-table
typedef u32 Identifier;

typedef struct Namespace Namespace;
typedef struct Procedure Procedure;
typedef struct ForStatement ForStatement;
typedef struct Struct Struct;
typedef struct Enum Enum;
typedef struct Typedef Typedef;
typedef struct ProcSignature ProcSignature;
typedef struct Datatype Datatype;
typedef struct ProcArg ProcArg;
typedef struct Declaration Declaration;
typedef struct Statement Statement;

typedef struct File {
    char* filename;
    Namespace* namespace;
} File;

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
    Typekind_Opaque,
    Typekind_Procedure
} Typekind;

typedef struct Datatype {
    Typekind kind;
    u32 numPointers;
    union {
        void* data_ptr;
        Identifier opaque_name;
        Struct* stru;
        Enum* _enum;
        Typedef* type_def;
        ProcSignature* procedure;
    };
} Datatype;


/*
    TODO: possible syntax tree hirarchy:
    Codebase
     > Namespace
      > File
       > Struct
       > Enum
       > Procedure
        > Scope
         > Statement
*/


typedef struct Node {
    u32 file_index;
    u32 lineNumber;
} Node;



typedef enum TypeNode {
    TypeNode_MustInfer,
    TypeNode_Normal,
    TypeNode_Pointer,
    TypeNode_Procedure
} TypeNode;

typedef struct Type {
    Node nodebase;
    TypeNode node_type;

    Datatype solvedstate;

    union {
        struct {
            Identifier namespace_name, name;
        };

        struct Type* pointedto;

        struct {
            struct Type* return_type;
            struct Type* arguments;
        } procedure;
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
#define type_bool    type_uint8;

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
    CompoundElement* elements;
} CompoundExpression;

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
    Type* type;
} AllocExpression;

typedef struct DerefOperator {
    Expression base;
    Expression* expr;
    Statement* ref;
    Identifier name;
} DerefOperator;

typedef struct VariableExpression {
    Expression base;
    Identifier name;

    Statement* ref;
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
    // Statement_LocalVariable,
    Statement_Declaration,
    Statement_GlobalVariable,
    Statement_FixedArray_Declaration,
    Statement_Constant,
    Statement_Typedef,
    Statement_Procedure,
    Statement_Argument,
    Statement_Struct,
    Statement_Enum,
    Statement_EnumEntry,
    Statement_Namespace,

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


/*
    type name = SomeType; // type definition      --
    const name = expr;    // compiletime constant -- type is null
    SomeType name = expr; // variable             -- type is not null
*/
typedef struct Declaration {
    Statement base;
    Type* type;
    Expression* expr; // if this is a fixed sized array expr is the size expression for the array
    Identifier name;
    bool include_context;
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

typedef struct ForStatement {
    Statement base;
    Type* index_type; // can be null
    Identifier index_name;
    Expression* min_expr;
    Expression* max_expr;
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

typedef struct Argument {
    Datatype type;
    Identifier name;
    struct Argument* next;
} Argument;

typedef struct ProcSignature {
    Datatype return_type;
    u32 arg_count;
    Argument* arguments;
} ProcSignature;

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

    Datatype ptr_type;
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


// ----Struct----------------------------------------------


typedef struct Struct {
    Statement base;
    Identifier name;
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

typedef struct Namespace {
    Statement base;
    Identifier name;

    Procedure* procedures; // list

    Declaration* declarations; // list

    Struct* structs; // list
    Enum* enums; // list
    Typedef* type_defs; // list
    Identifier* opaque_types; // list

} Namespace;

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
        en->entries[i]._enum = en; // TODO: sly trick!
        return &en->entries[i];
    }
    return null;
}

static Enum* getEnum(Namespace* ns, Identifier name) {
    foreach (en, ns->enums) {
        if (en->name == name) return en;
    }

    return null;
}



typedef struct Codebase {

    Namespace** namespaces; // list

    // DynamicBuffer* procedure_types;
    Arena arena_procedure_types;

    Arena arena_signatures;
    ProcSignature* proc_signatures;

    // TODO: might be a good idea to make the string table be a hashmap, every time we append a new string it must go through the entire table
    struct {
        u32* byteoffsets; // list
        DynamicBuffer* data;
    } string_table;

} Codebase;

static Codebase g_Codebase; // The current codebase that is being parsed/validated/transpiled

#define foreachNamespace(scope) { foreach (nsp, g_Codebase.namespaces) { Namespace* ns = *nsp; scope } }
#define iterate(collection, scope) foreachNamespace(foreach(item, ns->collection) {scope} )

static inline char* get_string(Identifier id) { return (char*)(&g_Codebase.string_table.data->bytes[id]); }
static inline char* get_string_byindex(u32 index) { return (char*)(&g_Codebase.string_table.data->bytes[g_Codebase.string_table.byteoffsets[index]]); }

static Identifier register_string(StrSpan word) {
    u32 len = list_length(g_Codebase.string_table.byteoffsets);
    for (u32 i = 0; i < len; i++) {
        u32 byteOffset = g_Codebase.string_table.byteoffsets[i];
        char* s = (char*)(&g_Codebase.string_table.data->bytes[byteOffset]);
        if (spanEquals(word, s)) return byteOffset;
    }

    u32 byteOffset = dyReserve(&g_Codebase.string_table.data, word.length + 1);
    u8* p = (&g_Codebase.string_table.data->bytes[byteOffset]);
    for (u32 i = 0; i < word.length; i++) p[i] = word.start[i];
    p[word.length] = '\0';

    list_add(g_Codebase.string_table.byteoffsets, byteOffset);
    return byteOffset;
}

static Namespace* namespace_create() {
    Namespace* ns = malloc(sizeof(Namespace));
    ns->name = 0;
    ns->base.statementType = Statement_Namespace;

    ns->procedures = list_create(Procedure);
    ns->declarations = list_create(Declaration);

    ns->structs = list_create(Struct);
    ns->enums = list_create(Enum);
    ns->type_defs = list_create(Typedef);
    ns->opaque_types = list_create(Identifier);

    return ns;
}

static Namespace* ensureNamespace(Identifier name) {
    foreach (item, g_Codebase.namespaces) {
        Namespace* ns = *item;
        if (ns->name == name) return ns;
    }

    Namespace* ns = namespace_create();
    ns->name = name;
    list_add(g_Codebase.namespaces, ns);
    return ns;
}

static Namespace* getNamespace(Identifier name) {
    foreach (item, g_Codebase.namespaces) {
        Namespace* ns = *item;
        if (ns->name == name) return ns;
    }

    return null;
}

static void codebase_init(Codebase* codebase) {
    codebase->namespaces = list_create(Namespace*);
    Namespace* default_ns = namespace_create();
    list_add(codebase->namespaces, default_ns);

    // codebase->procedure_types = dyCreate();
    codebase->arena_procedure_types = arena_create();
    codebase->arena_signatures = arena_create();


    codebase->string_table.byteoffsets = list_create(u32);
    codebase->string_table.data = dyCreate();
    register_string(spFrom("")); // empty string
}

static ProcSignature* createSignature(Datatype return_type) {
    ProcSignature* sig = arena_alloc(&g_Codebase.arena_signatures, sizeof(ProcSignature));
    sig->return_type = return_type;
    return sig;
}

static void addArgument(ProcSignature* sig, Datatype argtype, Identifier argname) {
    Argument* arg = arena_alloc(&g_Codebase.arena_signatures, sizeof(Argument));
    arg->name = argname;
    arg->type = argtype;

    if (sig->arguments) {
        Argument* last = sig->arguments;
        while (last->next) last = last->next;
        last->next = arg;
    } else {
        sig->arguments = arg;
    }

    sig->arg_count++;
}

static void createSignatureFromProcedure(Procedure* proc) {
    ProcSignature* sig = createSignature(proc->returnType->solvedstate);
    if (proc->arguments) {
        foreach (arg, proc->arguments) {
            addArgument(sig, arg->type->solvedstate, 0);
        }
    }

    proc->ptr_type = (Datatype) { .kind = Typekind_Procedure, .procedure = sig, .numPointers = 1 };
}

static Statement* getFromNamespace(Namespace* ns, Identifier name) {
    foreach (proc, ns->procedures) {
        if (proc->name == name) return (Statement*)proc;
    }

    foreach (decl, ns->declarations) {
        if (decl->name == name) return (Statement*)decl;
    }

    // TODO: opaque types?
    foreach (def, ns->type_defs) {
        if (def->name == name) return (Statement*)def;
    }

    foreach (stru, ns->structs) {
        if (stru->name == name) return (Statement*)stru;
    }

    foreach (en, ns->enums) {
        if (en->name == name) return (Statement*)en;
    }

    return null;
}

static bool isCompiletimeExpression(Expression* expr) {
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
            BinaryExpression* be = (BinaryExpression*)expr;
            return isCompiletimeExpression(be->left) && isCompiletimeExpression(be->right);

        case ExprType_Unary_PreIncrement:
        case ExprType_Unary_PostIncrement:
        case ExprType_Unary_PreDecrement:
        case ExprType_Unary_PostDecrement:
            return false;

        case ExprType_Unary_Not:
        case ExprType_Unary_BitwiseNot:
        case ExprType_Unary_Negate:
        case ExprType_Unary_ValueOf:
            UnaryExpression* un = (UnaryExpression*)expr;
            return isCompiletimeExpression(un->expr);

        case ExprType_Unary_AddressOf: return false;

        case ExprType_Literal_Integer:
        case ExprType_Literal_Decimal:
        case ExprType_Literal_Char:
        case ExprType_Literal_String:
        case ExprType_Literal_True:
        case ExprType_Literal_False:
        case ExprType_Literal_Null:
            return true;

        case ExprType_Variable:
            VariableExpression* var = (VariableExpression*)expr;
            // return var->reference.reftype == RefType_Constant;
            return var->ref->statementType == Statement_Constant;

        case ExprType_Alloc: return false;

        case ExprType_Ternary:
            TernaryExpression* ter = (TernaryExpression*)expr;
            return isCompiletimeExpression(ter->condition) && isCompiletimeExpression(ter->thenExpr) && isCompiletimeExpression(ter->elseExpr);

        case ExprType_ProcCall: return false;

        case ExprType_Deref:
            DerefOperator* deref = (DerefOperator*)expr;

            if (deref->ref) {
                if (deref->ref->statementType == Statement_EnumEntry) return true;
            }

            return isCompiletimeExpression(deref->expr);

        case ExprType_Indexing:
            IndexingExpression* ind = (IndexingExpression*)expr;
            return isCompiletimeExpression(ind->indexed) && isCompiletimeExpression(ind->index);

        case ExprType_Cast: return isCompiletimeExpression(((CastExpression*)expr)->expr);

        case ExprType_Sizeof: return true;

        case ExprType_Parenthesized: return isCompiletimeExpression(((ParenthesizedExpression*)expr)->innerExpr);
    }
}
