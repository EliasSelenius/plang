

typedef struct Value {
    void* data;
    Datatype type;
} Value;



static void interpretExpression(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Plus: break;
        case ExprType_Minus: break;
        case ExprType_Mul: break;
        case ExprType_Div: break;
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
        case ExprType_Literal_Integer: break;
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
        case ExprType_Parenthesized: break;
    }
}