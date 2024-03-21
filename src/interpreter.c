

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

#define value_invalid (Value) {0}


typedef struct ReplVariable {
    Identifier name;
    Value value;
} ReplVariable;

typedef struct ScopeContext {
    struct ScopeContext* enclosing_context;
    ReplVariable* variables; // list
} ScopeContext;



static Value interpret_expression(Expression* expr, ScopeContext* context);




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


static ScopeContext* push_scope(ScopeContext* enclosing) {
    ScopeContext* res = malloc(sizeof(ScopeContext));
    res->enclosing_context = enclosing;
    res->variables = list_create(ReplVariable);
    return res;
}

static ScopeContext* pop_scope(ScopeContext* context) {
    list_delete(context->variables);
    ScopeContext* res = context->enclosing_context;
    free(context);
    return res;
}

static ReplVariable* scope_context_get(ScopeContext* context, Identifier name) {
    foreach (var, context->variables) {
        if (var->name == name) return var;
    }

    if (context->enclosing_context) return scope_context_get(context->enclosing_context, name);
    return null;
}

static void scope_context_set(ScopeContext* context, Identifier name, Value v) {
    scope_context_get(context, name)->value = v;
}

static void scope_context_declare(ScopeContext* context, Identifier name, Value v) {

    // TODO: already declared error? or maybe this is already handled by parser

    ReplVariable var = {name, v};
    list_add(context->variables, var);
}

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


static Value interpret_procedure(ProcCall* call, ScopeContext* context) {
    if (call->proc->name == builtin_string_print) {
        if (call->args) {
            foreach (arg, call->args) {
                print_value(interpret_expression(*arg, context));
            }
        }
    }

    return value_invalid;
}

static Value interpret_expression(Expression* expr, ScopeContext* context) {
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
            Value left = interpret_expression(e.binary->left, context);   \
            Value right = interpret_expression(e.binary->right, context); \
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
            Value v = interpret_expression(e.unary->expr, context);
            v.uint64 = !v.uint64;
            return v;
        }
        case ExprType_Unary_BitwiseNot: {
            Value v = interpret_expression(e.unary->expr, context);
            v.uint64 = ~v.uint64;
            return v;
        }
        case ExprType_Unary_AddressOf: break;
        case ExprType_Unary_ValueOf: break;
        case ExprType_Unary_Negate: {
            Value v = interpret_expression(e.unary->expr, context);
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
            if (e.var->ref == null) {
                ReplVariable* var = scope_context_get(context, e.var->name);
                if (var) return var->value;

                printf("Could not find reference\n");
            }

            

        } break;
        case ExprType_Alloc: {
            u64 size = datatype_bytesize(e.alloc->type->solvedstate);
            Value v = {0};
            v.pointer = malloc(size);
            v.type = expr->datatype;
            return v;
        }
        case ExprType_Ternary: {
            Value c = interpret_expression(e.ternary->condition, context);
            if (c.pointer) return interpret_expression(e.ternary->thenExpr, context);
            return interpret_expression(e.ternary->elseExpr, context);
        }
        case ExprType_ProcCall: {
            return interpret_procedure(e.call, context);
        }
        case ExprType_Deref: break;
        case ExprType_Indexing: break;
        case ExprType_Cast: break;
        case ExprType_Sizeof: return (Value) { .type = type_ambiguousInteger, .uint64 = datatype_bytesize(e.size_of->type->solvedstate) };
        case ExprType_Parenthesized: return interpret_expression(e.parenth->innerExpr, context);
        case ExprType_Compound: break;
    }

    return value_invalid;
}


static Value interpret_statement(Statement* sta, ScopeContext* context) {
    StmtPointer s = (StmtPointer)sta;

    switch (sta->statementType) {
        case Statement_Declaration: {
            Value v = {0};
            if (s.decl->expr) v = interpret_expression(s.decl->expr, context);
            scope_context_declare(context, s.decl->name, v);
        } break;
        case Statement_Constant: break;
        case Statement_Typedef: break;
        case Statement_Procedure: break;
        case Statement_Argument: break;
        case Statement_Struct: break;
        case Statement_Enum: break;
        case Statement_EnumEntry: break;
        case Statement_Assignment: {
        } break;
        case Statement_Expression: {
            return interpret_expression(s.expr->expr, context);
        } break;
        case Statement_Scope: {
            ScopeContext* inner_scope = push_scope(context);
            foreach (sta, s.scope->statements) {
                interpret_statement(*sta, inner_scope);
            }
            pop_scope(inner_scope);
        } break;
        case Statement_If: {
            Value v = interpret_expression(s.if_sta->condition, context);
            if (v.pointer) interpret_statement(s.if_sta->then_statement, context);
            else interpret_statement(s.if_sta->else_statement, context);
        } break;
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

    return (Value) { .type = type_invalid, .pointer = null };
}


void repl_init(REPL* repl, Codebase* cb) {
    repl->codebase = cb;
    repl->context = push_scope(null);
    repl->parser = init_parser();
    repl->parser->allow_omitting_semicolon = true;
}

u64 repl_input(char* code, REPL* repl) {
    lex(repl->parser, code);
    reset_parser(repl->parser);
    Statement* sta = expect_node(repl->parser);

    foreach (var, repl->parser->unresolved_variables) {
        (*var)->ref = get_global_symbol_from_codebase((*var)->name, repl->codebase);
        // if ((*var)->ref == null) printf("unresolved var\n");
    }

    validateStatement(repl->parser, sta);

    Value v = interpret_statement(sta, repl->context);
    if (sta->statementType == Statement_Expression) {
        print_value(v);
        printf("\n");
    }

    return v.uint64;
}