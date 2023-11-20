

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

    char* type_str = construct_type_string(value.type, temp_builder());
    printf("(%s)", type_str);

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


typedef struct REPL_Variable {
    Identifier name;
    Value value;
} REPL_Variable;

typedef struct REPL {
    Codebase* codebase; // can be null
    REPL_Variable* variables; // list
} REPL;

static u64 datatype_bytesize(Datatype type) {
    if (type.numPointers) return 8;

    switch (type.kind) {

        case Typekind_uint8:  return 1;
        case Typekind_uint16: return 2;
        case Typekind_uint32: return 4;
        case Typekind_uint64: return 8;
        case Typekind_int8:   return 1;
        case Typekind_int16:  return 2;
        case Typekind_int32:  return 4;
        case Typekind_int64:  return 8;
        case Typekind_float32: return 4;
        case Typekind_float64: return 8;
        case Typekind_void: return 0;
        case Typekind_char: return 1;
        case Typekind_Struct: return 0;
        case Typekind_Enum: return 0;
        case Typekind_Typedef: return 0;
        case Typekind_Procedure: return 8;
        case Typekind_Array: return 12;
        case Typekind_Fixed_Array: return 0;
        case Typekind_Dynamic_Array: return 8;

        default: return 0;
    }
}

static Value interpret_expression(Expression* expr) {
    ExprPointer e = (ExprPointer)expr;

    switch (expr->expressionType) {

        #define integer(op) \
                case Typekind_uint8:   value.uint8   = left.uint8   op right.uint8;   break; \
                case Typekind_uint16:  value.uint16  = left.uint16  op right.uint16;  break; \
                case Typekind_uint32:  value.uint32  = left.uint32  op right.uint32;  break; \
                case Typekind_uint64:  value.uint64  = left.uint64  op right.uint64;  break; \
                case Typekind_int8:    value.int8    = left.int8    op right.int8;    break; \
                case Typekind_int16:   value.int16   = left.int16   op right.int16;   break; \
                case Typekind_int32:   value.int32   = left.int32   op right.int32;   break; \
                case Typekind_int64:   value.int64   = left.int64   op right.int64;   break; \
                case Typekind_AmbiguousInteger: value.uint64 = left.uint64 op right.uint64; break;

        #define numeric(op) \
                case Typekind_uint8:   value.uint8   = left.uint8   op right.uint8;   break; \
                case Typekind_uint16:  value.uint16  = left.uint16  op right.uint16;  break; \
                case Typekind_uint32:  value.uint32  = left.uint32  op right.uint32;  break; \
                case Typekind_uint64:  value.uint64  = left.uint64  op right.uint64;  break; \
                case Typekind_int8:    value.int8    = left.int8    op right.int8;    break; \
                case Typekind_int16:   value.int16   = left.int16   op right.int16;   break; \
                case Typekind_int32:   value.int32   = left.int32   op right.int32;   break; \
                case Typekind_int64:   value.int64   = left.int64   op right.int64;   break; \
                case Typekind_float32: value.float32 = left.float32 op right.float32; break; \
                case Typekind_float64: value.float64 = left.float64 op right.float64; break; \
                case Typekind_AmbiguousInteger: value.uint64 = left.uint64 op right.uint64; break; \
                case Typekind_AmbiguousDecimal: value.float64 = left.float64 op right.float64; break;

        #define binary_expr(cases) {                             \
            Value left = interpret_expression(e.binary->left);   \
            Value right = interpret_expression(e.binary->right); \
            Value value = {0};                                   \
            value.type = expr->datatype;                         \
            switch (expr->datatype.kind) {                       \
                cases                                            \
                default: break;                                  \
            }                                                    \
            return value;                                        \
        }

        case ExprType_Plus:           binary_expr(numeric(+))
        case ExprType_Minus:          binary_expr(numeric(-))
        case ExprType_Mul:            binary_expr(numeric(*))
        case ExprType_Div:            binary_expr(numeric(/))
        case ExprType_Mod:            binary_expr(integer(%))
        case ExprType_Less:           binary_expr(numeric(<))
        case ExprType_Greater:        binary_expr(numeric(>))
        case ExprType_LessEquals:     binary_expr(numeric(<=))
        case ExprType_GreaterEquals:  binary_expr(numeric(>=))
        case ExprType_Equals:         binary_expr(numeric(==))
        case ExprType_NotEquals:      binary_expr(numeric(!=))
        case ExprType_BooleanAnd:     binary_expr(numeric(&&)) // TODO: boolean and & or change control-flow, the right-hand side of the expression should not be interpreted in certain cases
        case ExprType_BooleanOr:      binary_expr(numeric(||))
        case ExprType_Bitwise_And:    binary_expr(integer(&))
        case ExprType_Bitwise_Or:     binary_expr(integer(|))
        case ExprType_Bitwise_Xor:    binary_expr(integer(^))
        case ExprType_Bitwise_Lshift: binary_expr(integer(<<))
        case ExprType_Bitwise_Rshift: binary_expr(integer(>>))

        #undef binary_expr
        #undef integer
        #undef numeric

        case ExprType_Unary_PreIncrement: break;
        case ExprType_Unary_PostIncrement: break;
        case ExprType_Unary_PreDecrement: break;
        case ExprType_Unary_PostDecrement: break;


        case ExprType_Unary_Not: {
            Value v = interpret_expression(e.unary->expr);
            v.uint64 = !v.uint64;
            return v;
        }
        case ExprType_Unary_BitwiseNot: {
            Value v = interpret_expression(e.unary->expr);
            v.uint64 = ~v.uint64;
            return v;
        }
        case ExprType_Unary_AddressOf: break;
        case ExprType_Unary_ValueOf: break;
        case ExprType_Unary_Negate: {
            Value v = interpret_expression(e.unary->expr);
            v.uint64 = -v.uint64;
            return v;
        }


        case ExprType_Literal_Integer: return (Value) { .type = type_ambiguousInteger, .uint64 = e.literal->data.integer };
        case ExprType_Literal_Decimal: return (Value) { .type = type_ambiguousDecimal, .float64 = e.literal->data.decimal };
        case ExprType_Literal_Char:    return (Value) { .type = type_char, .character = e.literal->data.character };
        case ExprType_Literal_String:  return (Value) { .type = type_charPointer, .pointer = get_string(e.literal->data.string) };
        case ExprType_Literal_True:    return (Value) { .type = type_bool, .uint64 = true };
        case ExprType_Literal_False:   return (Value) { .type = type_bool, .uint64 = false };
        case ExprType_Literal_Null:    return (Value) { .type = type_voidPointer, .pointer = null };

        case ExprType_Variable: {
            // if (e.var->ref)
        } break;
        case ExprType_Alloc: {
            u64 size = datatype_bytesize(e.alloc->type->solvedstate);
            Value v = {0};
            v.pointer = malloc(size);
            v.type = expr->datatype;
            return v;
        }
        case ExprType_Ternary: {
            Value c = interpret_expression(e.ternary->condition);
            if (c.pointer) return interpret_expression(e.ternary->thenExpr);
            return interpret_expression(e.ternary->elseExpr);
        }
        case ExprType_ProcCall: break;
        case ExprType_Deref: break;
        case ExprType_Indexing: break;
        case ExprType_Cast: break;
        case ExprType_Sizeof: return (Value) { .type = type_ambiguousInteger, .uint64 = datatype_bytesize(e.size_of->type->solvedstate) };
        case ExprType_Parenthesized: return interpret_expression(e.parenth->innerExpr);
        case ExprType_Compound: break;
    }

    return (Value) { .type = type_invalid, .pointer = null };
}


static void interpret_statement(Statement* sta, REPL* repl) {
    StmtPointer s = (StmtPointer)sta;

    switch (sta->statementType) {
        case Statement_Declaration: break;
        case Statement_Constant: break;
        case Statement_Typedef: break;
        case Statement_Procedure: break;
        case Statement_Argument: break;
        case Statement_Struct: break;
        case Statement_Enum: break;
        case Statement_EnumEntry: break;
        case Statement_Assignment: break;
        case Statement_Expression: break;
        case Statement_Scope: break;
        case Statement_If: break;
        case Statement_While: break;
        case Statement_For: break;
        case Statement_Switch: break;
        case Statement_Continue: break;
        case Statement_Break: break;
        case Statement_Return: break;
        case Statement_Goto: break;
        case Statement_Label: break;
        case Statement_CaseLabel: break;
        case Statement_DefaultLabel: break;
    }
}


u64 repl_input(char* code, Codebase* cb) {
    lex(code);
    reset_parser();
    Expression* expr = expectExpression();

    foreach (var, parser.unresolved_variables) {
        (*var)->ref = get_global_symbol_from_codebase((*var)->name, cb);
        if ((*var)->ref == null) printf("unresolved var\n");
    }

    validateExpression(expr);

    Value v = interpret_expression(expr);
    print_value(v);
    printf("\n");
    return v.uint64;
}