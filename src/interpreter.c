

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


static void print_value(Value value) {

    if (value.type.numPointers == 1) {
        if (value.type.kind == Typekind_char) {
            printf("%s", (char*)value.pointer);
            return;
        }
    }

    if (value.type.numPointers) {
        printf("%p", value.pointer);
        return;
    }

    switch (value.type.kind) {
        case Typekind_Invalid:          printf("invalid(%p)", value.pointer); break;
        case Typekind_AmbiguousInteger: printf("%lld", value.int64); break;
        case Typekind_AmbiguousDecimal: printf("%f", value.float64); break;
        case Typekind_uint8:            printf("%u", (u32)value.uint8); break;
        case Typekind_uint16:           printf("%hu", value.uint16); break;
        case Typekind_uint32:           printf("%u", value.uint32); break;
        case Typekind_uint64:           printf("%llu", value.uint64); break;
        case Typekind_int8:             printf("%d", (i32)value.int8); break;
        case Typekind_int16:            printf("%hd", value.int16); break;
        case Typekind_int32:            printf("%d", value.int32); break;
        case Typekind_int64:            printf("%lld", value.int64); break;
        case Typekind_float32:          printf("%f", value.float32); break;
        case Typekind_float64:          printf("%f", value.float64); break;
        case Typekind_void:             printf("void"); break;
        case Typekind_char:             printf("%c", value.character); break;
        case Typekind_Struct: break;
        case Typekind_Enum: break;
        case Typekind_Typedef: break;
        case Typekind_Procedure: break;
        case Typekind_Array: break;
        case Typekind_Fixed_Array: break;
        case Typekind_Dynamic_Array: break;
    }
}


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
        case ExprType_Literal_Integer: return (Value) { .type = type_ambiguousInteger, .uint64 = e.literal->data.integer };
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
        case ExprType_Compound: break;
    }

    return (Value) { .type = type_invalid, .pointer = null };
}