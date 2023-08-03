

typedef struct Value {
    Datatype type;

    union {
        void* pointer;
        u8 uint8;
        u16 uint16;
        u32 uint32;
        u64 uint64;
        i8 int8;
        i16 int16;
        i32 int32;
        i64 int64;
        f32 float32;
        f64 float64;
        char character;
    };
} Value;


typedef union ExprPointer {
    Expression* expr;
    BinaryExpression* binary;
    UnaryExpression* unary;
    LiteralExpression* literal;
    ParenthesizedExpression* parenth;
} ExprPointer;


static Value interpret_expression(Expression* expr) {

    ExprPointer e = (ExprPointer)expr;


    switch (expr->expressionType) {

        #define binary_expr(op) {\
            Value left = interpret_expression(e.binary->left);\
            Value right = interpret_expression(e.binary->right);\
            Value v = { .type = expr->datatype, .int64 = left.int64 op right.int64 };\
            return v; }\

        case ExprType_Plus: binary_expr(+)
        case ExprType_Minus: binary_expr(-)
        case ExprType_Mul: binary_expr(*)
        case ExprType_Div: binary_expr(/)

        #undef binary_expr

        case ExprType_Mod: break;
        case ExprType_Less: break;
        case ExprType_Greater: break;
        case ExprType_LessEquals: break;
        case ExprType_GreaterEquals: break;
        case ExprType_Equals: break;
        case ExprType_NotEquals: break;
        case ExprType_BooleanAnd: break;
        case ExprType_BooleanOr: break;
        case ExprType_Bitwise_And: break;
        case ExprType_Bitwise_Or: break;
        case ExprType_Bitwise_Xor: break;
        case ExprType_Bitwise_Lshift: break;
        case ExprType_Bitwise_Rshift: break;
        case ExprType_Unary_PreIncrement: break;
        case ExprType_Unary_PostIncrement: break;
        case ExprType_Unary_PreDecrement: break;
        case ExprType_Unary_PostDecrement: break;
        case ExprType_Unary_Not: break;
        case ExprType_Unary_BitwiseNot: break;
        case ExprType_Unary_AddressOf: break;
        case ExprType_Unary_ValueOf: break;
        case ExprType_Unary_Negate: break;
        case ExprType_Literal_Integer: return (Value) { .type = (Datatype) {Typekind_AmbiguousInteger}, .uint64 = e.literal->integer };
        case ExprType_Literal_Decimal: break;
        case ExprType_Literal_Char: break;
        case ExprType_Literal_String: break;
        case ExprType_Literal_True: break;
        case ExprType_Literal_False: break;
        case ExprType_Literal_Null: break;
        case ExprType_Variable: break;
        case ExprType_Alloc: break;
        case ExprType_Ternary: break;
        case ExprType_ProcCall: break;
        case ExprType_Deref: break;
        case ExprType_Indexing: break;
        case ExprType_Cast: break;
        case ExprType_Sizeof: break;
        case ExprType_Parenthesized: return interpret_expression(e.parenth->innerExpr);
    }

    return (Value) { .type = type_invalid, .pointer = null };
}