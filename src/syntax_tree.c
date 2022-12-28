

// signifies a string thats stored in the string-table
typedef u32 Identifier;
typedef struct NamespacedID { Identifier namespace_name, name; } NamespacedID;


// ----Types---------------------------------------------

typedef enum Typekind {

    Typekind_Invalid = 0, // used by validator to signify a type that could not be determined because of an error
    Typekind_Undecided, // used by parser to mean either struct, enum or alias
    Typekind_MustBeInfered,
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
    Typekind_Alias,
    Typekind_Opaque,
    Typekind_ProcPtr
} Typekind;

typedef struct Datatype {
    Typekind kind;
    /*
        ref:
            Typekind_Undecided  -> Identifier of struct/enum/alias
            Typekind_Struct     -> index into structs
            Typekind_Alias      -> index into aliases
            Typekind_Opaque     -> Identifier
            Typekind_ProcPtr    -> byteoffset into procptrs buffer
    */
    union {
        NamespacedID id;
        u32 ref;
    };

    u32 numPointers;
} Datatype;

typedef struct AliasType {
    u32 name;
    Datatype aliasedType;
} AliasType;

#define type_invalid          (Datatype) { .kind = Typekind_Invalid, .ref = 0, .numPointers = 0 }
#define type_void             (Datatype) { .kind = Typekind_void, .ref = 0, .numPointers = 0 }
#define type_voidPointer      (Datatype) { .kind = Typekind_void, .ref = 0, .numPointers = 1 }
#define type_char             (Datatype) { .kind = Typekind_char, .ref = 0, .numPointers = 0 }
#define type_charPointer      (Datatype) { .kind = Typekind_char, .ref = 0, .numPointers = 1 }
#define type_int32            (Datatype) { .kind = Typekind_int32, .ref = 0, .numPointers = 0 }
#define type_uint32           (Datatype) { .kind = Typekind_uint32, .ref = 0, .numPointers = 0 }
#define type_int64            (Datatype) { .kind = Typekind_int64, .ref = 0, .numPointers = 0 }
#define type_uint64           (Datatype) { .kind = Typekind_uint64, .ref = 0, .numPointers = 0 }
#define type_float32          (Datatype) { .kind = Typekind_float32, .ref = 0, .numPointers = 0 }
#define type_float64          (Datatype) { .kind = Typekind_float64, .ref = 0, .numPointers = 0 }
#define type_ambiguousInteger (Datatype) { .kind = Typekind_AmbiguousInteger, .ref = 0, .numPointers = 0 }
#define type_ambiguousDecimal (Datatype) { .kind = Typekind_AmbiguousDecimal, .ref = 0, .numPointers = 0 }

static inline bool typeEquals(Datatype a, Datatype b) {
    return a.kind == b.kind && a.ref == b.ref && a.numPointers == b.numPointers;
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
    return (Datatype) {
        .kind = getTypekindOfNumberInfo(mergeNumberInfos(getNumberInfo(a.kind), getNumberInfo(b.kind))),
        .ref = 0,
        .numPointers = 0
    };
}

typedef struct Node {
    u32 lineNumber;
    char* filepath;
} Node;


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
    ExprType_Alloc,
    ExprType_Ternary,
    ExprType_ProcCall,
    ExprType_Deref,
    ExprType_Indexing,
    ExprType_Cast,
    ExprType_Sizeof,

    ExprType_Parenthesized

} ExprType;

typedef struct Expression {
    Node nodebase;
    ExprType expressionType;
    Datatype datatype;
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
    Identifier name;
} DerefOperator;

typedef struct VariableExpression {
    Expression base;
    Identifier namespace_name;
    Identifier name;
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



// ----Statements----------------------------------------------


typedef enum StatementType {
    Statement_Declaration,
    Statement_FixedArray_Declaration,
    Statement_Assignment,
    Statement_Expression,

    Statement_Scope,
    Statement_If,
    Statement_While,
    Statement_ForIn,
    Statement_For,
    Statement_Switch,
    Statement_LocalProc,

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

typedef struct VarDecl {
    Statement base;
    Datatype type;
    Identifier name;
    Expression* assignmentOrNull;
} VarDecl;

typedef struct Assignement {
    Statement base;
    Expression* assigneeExpr;
    TokenType assignmentOper; // = += -= *= /= etc...
    Expression* expr;
} Assignement;

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

typedef struct ForInStatement {
    Statement base;
    Datatype index_type;
    Identifier index_name;
    Expression* min_expr;
    Expression* max_expr;
    Statement* statement;
} ForInStatement;

typedef struct ForStatement {
    Statement base;

    union {
        VarDecl* decl;
        Expression* expr;
    } init;

    Expression* test_expression;
    Expression* update_expression;
} ForStatement;

typedef struct IfStatement {
    Statement base;
    Expression* condition; // if condition is null, this is an else-statement
    Statement* statement;
    struct IfStatement* next;
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
} CaseLabelStatement;

// ----Procedures----------------------------------------------

typedef struct ProcPtr {
    Datatype returnType;
    u32 argCount;
    Datatype argTypes[];
} ProcPtr;

typedef struct ProcArg {
    Datatype type;
    Identifier name;
} ProcArg;

typedef struct ProcSignature {
    Datatype return_type;
    ProcArg* arguments; // list
} ProcSignature;

typedef struct Procedure {
    Identifier name;
    Datatype returnType;
    ProcArg* arguments; // list, can be null

    /* overload
        value of zero means this function is not overloaded.
        a value of N means this function is the N'th function in the set of all its siblings. Where N > 0
    */
    u32 overload;

    Scope* scope; // scope can be null

    Datatype ptr_type;
} Procedure;

typedef struct ProcCall {
    Expression base;
    Expression* proc_expr;
    Expression** args; // list of Expression pointers
    u32 overload;
} ProcCall;

typedef struct CapturedVariable {
    Identifier name;
    Datatype type;
} CapturedVariable;

typedef struct LocalProc {
    Statement base;
    Procedure proc;
    CapturedVariable* captures; // list
} LocalProc;

// ----Struct----------------------------------------------

typedef struct Field {
    Node nodebase;
    Datatype type;
    Identifier name;
} Field;

typedef struct PlangStruct {
    Node nodebase;
    Identifier name;
    Field* fields; // list
} PlangStruct;

static Field* getField(PlangStruct* stru, Identifier name) {
    u32 len = list_length(stru->fields);
    for (u32 i = 0; i < len; i++)
        if (stru->fields[i].name == name) return &stru->fields[i];
    return null;
}

typedef struct Constant {
    Identifier name;
    Expression* expr;
} Constant;

// ----Namespaces----------------------------------------------

typedef struct Namespace {
    Identifier name;

    Procedure* procedures; // list
    VarDecl* global_variables; // list
    Constant* constants; // list

    PlangStruct* structs; // list
    AliasType* aliases; // list
    Identifier* opaque_types; // list

} Namespace;

typedef struct Codebase {

    Namespace** namespaces; // list

    DynamicBuffer* procedure_types;

    // TODO: might be a good idea to make the string table be a hashmap, every time we append a new string it must go through the entire table
    struct {
        u32* byteoffsets; // list
        DynamicBuffer* data;
    } string_table;

} Codebase;

static Codebase g_Codebase; // The current codebase that is being parsed/validated/transpiled

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

    ns->procedures = list_create(Procedure);

    ns->global_variables = list_create(VarDecl);
    ns->constants = list_create(Constant);

    ns->structs = list_create(PlangStruct);
    ns->aliases = list_create(AliasType);
    ns->opaque_types = list_create(Identifier);

    return ns;
}

static Namespace* getNamespace(Identifier name) {
    foreach (item, g_Codebase.namespaces) {
        Namespace* ns = *item;
        if (ns->name == name) return ns;
    }

    Namespace* ns = namespace_create();
    ns->name = name;
    list_add(g_Codebase.namespaces, ns);
    return ns;
}

static void codebase_init(Codebase* codebase) {
    codebase->namespaces = list_create(Namespace*);
    Namespace* default_ns = namespace_create();
    list_add(g_Codebase.namespaces, default_ns);

    codebase->procedure_types = dyCreate();

    codebase->string_table.byteoffsets = list_create(u32);
    codebase->string_table.data = dyCreate();
    register_string(spFrom("")); // empty string
}

// get procedure, global or const
static Datatype getThing(Namespace* ns, Identifier name) {

    foreach (proc, ns->procedures) {
        if (proc->name == name) return proc->ptr_type;
    }

    foreach (glob, ns->global_variables) {
        if (glob->name == name) return glob->type;
    }

    foreach (cont, ns->constants) {
        if (cont->name == name) return cont->expr->datatype;
    }

    return type_invalid;
}

static void ensureProcSignature() {

}

static inline ProcPtr* getProcPtr(u32 id) { return (ProcPtr*)&g_Codebase.procedure_types->bytes[id]; }

static Datatype ensureProcPtr(Procedure* proc) {

    u32 argCount = proc->arguments ? list_length(proc->arguments) : 0;

    u32 length = g_Codebase.procedure_types->length;
    u32 procId = 0;
    while (procId < length) {
        ProcPtr* ptr = getProcPtr(procId);

        if (ptr->argCount == argCount && typeEquals(proc->returnType, ptr->returnType)) {
            for (u32 i = 0; i < argCount; i++)
                if (!typeEquals(proc->arguments[i].type, ptr->argTypes[i])) goto skip;
            return (Datatype) {
                .kind = Typekind_ProcPtr,
                .ref = procId,
                .numPointers = 1
            };
        }

        skip: procId += sizeof(ProcPtr) + sizeof(Datatype) * ptr->argCount;
    }


    procId = dyReserve(&g_Codebase.procedure_types, sizeof(ProcPtr));
    ProcPtr* ptr = getProcPtr(procId);
    ptr->returnType = proc->returnType;
    ptr->argCount = argCount;
    for (u32 i = 0; i < argCount; i++) {
        u32 argRef = dyReserve(&g_Codebase.procedure_types, sizeof(Datatype));
        *(Datatype*)(&g_Codebase.procedure_types->bytes[argRef]) = proc->arguments[i].type;
    }


    return (Datatype) {
        .kind = Typekind_ProcPtr,
        .ref = procId,
        .numPointers = 1
    };
}