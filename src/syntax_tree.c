

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
    Typekind_FuncPtr
} Typekind;

typedef struct Datatype {
    Typekind kind;
    /*
        ref:
            Typekind_Undecided  -> Identifier of struct/enum/alias
            Typekind_Struct     -> index into structs
            Typekind_Alias      -> index into aliases
            Typekind_Opaque     -> Identifier
    */
    u32 ref;
    u32 numPointers;
} Datatype;

#define type_invalid     (Datatype) { Typekind_Invalid, 0, 0 }
#define type_void        (Datatype) { Typekind_void, 0, 0 }
#define type_voidPointer (Datatype) { Typekind_void, 0, 1 }
#define type_char        (Datatype) { Typekind_char, 0, 0 }
#define type_charPointer (Datatype) { Typekind_char, 0, 1 }
#define type_int32       (Datatype) { Typekind_int32, 0, 0 }
#define type_uint32      (Datatype) { Typekind_uint32, 0, 0 }
#define type_int64       (Datatype) { Typekind_int64, 0, 0 }
#define type_uint64      (Datatype) { Typekind_uint64, 0, 0 }
#define type_float32     (Datatype) { Typekind_float32, 0, 0 }
#define type_float64     (Datatype) { Typekind_float64, 0, 0 }
#define type_ambiguousInteger (Datatype) { Typekind_AmbiguousInteger, 0, 0 }
#define type_ambiguousDecimal (Datatype) { Typekind_AmbiguousDecimal, 0, 0 }

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


typedef struct Node {
    u32 lineNumber;
    char* filepath;
} Node;

// signifies a string thats stored in the string-table
typedef u32 Identifier;

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



// ----Statements----------------------------------------------


typedef enum StatementType {
    Statement_Declaration,
    Statement_FixedArray_Declaration,
    Statement_Assignment,
    Statement_Expression,

    Statement_Scope,
    Statement_If,
    Statement_While,
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

// TODO: is it necessary to have both Codeblock and Scope?
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
    Statement* statement;
} WhileStatement;

typedef struct IfStatement {
    Statement base;
    Expression* condition; // if condition is null, this is an else-statement
    Statement* statement;
    struct IfStatement* next;
} IfStatement;

typedef struct SwitchStatement {
    Statement base;
    Expression* expr;
    Codeblock scope;
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



// ----Allocator----------------------------------------------

u32 ExpressionType_Bytesizes[] = {
    [ExprType_Plus] = sizeof(BinaryExpression),
    [ExprType_Minus] = sizeof(BinaryExpression),
    [ExprType_Mul] = sizeof(BinaryExpression),
    [ExprType_Div] = sizeof(BinaryExpression),
    [ExprType_Mod] = sizeof(BinaryExpression),
    [ExprType_Less] = sizeof(BinaryExpression),
    [ExprType_Greater] = sizeof(BinaryExpression),
    [ExprType_LessEquals] = sizeof(BinaryExpression),
    [ExprType_GreaterEquals] = sizeof(BinaryExpression),
    [ExprType_Equals] = sizeof(BinaryExpression),
    [ExprType_NotEquals] = sizeof(BinaryExpression),
    [ExprType_BooleanAnd] = sizeof(BinaryExpression),
    [ExprType_BooleanOr] = sizeof(BinaryExpression),
    [ExprType_Bitwise_And] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Or] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Xor] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Lshift] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Rshift] = sizeof(BinaryExpression),
    [ExprType_Unary_PreIncrement] = sizeof(UnaryExpression),
    [ExprType_Unary_PostIncrement] = sizeof(UnaryExpression),
    [ExprType_Unary_PreDecrement] = sizeof(UnaryExpression),
    [ExprType_Unary_PostDecrement] = sizeof(UnaryExpression),
    [ExprType_Unary_Not] = sizeof(UnaryExpression),
    [ExprType_Unary_BitwiseNot] = sizeof(UnaryExpression),
    [ExprType_Unary_AddressOf] = sizeof(UnaryExpression),
    [ExprType_Unary_ValueOf] = sizeof(UnaryExpression),
    [ExprType_Unary_Negate] = sizeof(UnaryExpression),
    [ExprType_Literal_Integer] = sizeof(LiteralExpression),
    [ExprType_Literal_Decimal] = sizeof(LiteralExpression),
    [ExprType_Literal_Char] = sizeof(LiteralExpression),
    [ExprType_Literal_String] = sizeof(LiteralExpression),
    [ExprType_Literal_True] = sizeof(LiteralExpression),
    [ExprType_Literal_False] = sizeof(LiteralExpression),
    [ExprType_Literal_Null] = sizeof(LiteralExpression),
    [ExprType_Variable] = sizeof(VariableExpression),
    [ExprType_Constant] = sizeof(VariableExpression),
    [ExprType_Alloc] = sizeof(AllocExpression),
    [ExprType_Ternary] = sizeof(TernaryExpression),
    [ExprType_FuncCall] = sizeof(FuncCall),
    [ExprType_FuncPointerCall] = sizeof(FuncCall),
    [ExprType_Deref] = sizeof(DerefOperator),
    [ExprType_Indexing] = sizeof(IndexingExpression),
    [ExprType_Cast] = sizeof(CastExpression),
    [ExprType_Sizeof] = sizeof(SizeofExpression),
    [ExprType_Parenthesized] = sizeof(ParenthesizedExpression)
};

u32 StatementType_Bytesizes[] = {
    [Statement_Declaration] = sizeof(VarDecl),
    [Statement_FixedArray_Declaration] = sizeof(VarDecl),
    [Statement_Assignment] = sizeof(Assignement),
    [Statement_Expression] = sizeof(StatementExpression),
    [Statement_Scope] = sizeof(Scope),
    [Statement_If] = sizeof(IfStatement),
    [Statement_While] = sizeof(WhileStatement),
    [Statement_Switch] = sizeof(SwitchStatement),
    [Statement_Continue] = sizeof(Statement),
    [Statement_Break] = sizeof(Statement),
    [Statement_Return] = sizeof(ReturnStatement),
    [Statement_Goto] = sizeof(GotoStatement),
    [Statement_Label] = sizeof(LabelStatement),
    [Statement_CaseLabel] = sizeof(CaseLabelStatement),
    [Statement_DefaultLabel] = sizeof(Statement)
};

void* allocate_node(u32 node_size) {
    Node* node = calloc(1, node_size);
    node->filepath = g_Filename;
    node->lineNumber = tokens[token_index].line;
    return node;
}

void* allocExpr(ExprType type) {
    Expression* expr = allocate_node(ExpressionType_Bytesizes[type]);
    expr->expressionType = type;
    expr->datatype = type_invalid;
    return expr;
}

void* allocStatement(StatementType type) {
    Statement* sta = allocate_node(StatementType_Bytesizes[type]);
    sta->statementType = type;
    return sta;
}