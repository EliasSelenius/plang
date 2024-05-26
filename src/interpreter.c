

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

typedef struct ReplScope {
    struct ReplScope* enclosing_context;
    ReplVariable* variables; // list
} ReplScope;



static Value interpret_expression(NodeRef e, ReplScope* context);




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


static ReplScope* push_scope(ReplScope* enclosing) {
    ReplScope* res = malloc(sizeof(ReplScope));
    res->enclosing_context = enclosing;
    res->variables = list_create(ReplVariable);
    return res;
}

static ReplScope* pop_scope(ReplScope* context) {
    list_delete(context->variables);
    ReplScope* res = context->enclosing_context;
    free(context);
    return res;
}

static ReplVariable* scope_context_get(ReplScope* context, Identifier name) {
    foreach (var, context->variables) {
        if (var->name == name) return var;
    }

    if (context->enclosing_context) return scope_context_get(context->enclosing_context, name);
    return null;
}

static void scope_context_set(ReplScope* context, Identifier name, Value v) {
    scope_context_get(context, name)->value = v;
}

static void scope_context_declare(ReplScope* context, Identifier name, Value v) {

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


static Value interpret_procedure(ProcCallExpression* call, ReplScope* context) {
    if (call->proc->name == builtin_string_print) {
        if (call->args) {
            foreach (arg, call->args) {
                print_value(interpret_expression(*arg, context));
            }
        }
    }

    return value_invalid;
}

static Value interpret_expression(NodeRef e, ReplScope* context) {
    switch (e.node->kind) {

        #define C(type, op) case Typekind_##type: value.type = left.type op right.type; break;

        #define integer(op) \
            C(uint8, op) C(uint16, op) C(uint32, op) C(uint64, op)\
            C(int8,  op) C(int16,  op) C(int32,  op) C(int64,  op)\
            case Typekind_AmbiguousInteger: value.uint64 = left.uint64 op right.uint64; break;

        #define numeric(op) \
            integer(op) C(float32, op) C(float64, op) \
            case Typekind_AmbiguousDecimal: value.float64 = left.float64 op right.float64; break;

        #define binary_expr(cases) {                                       \
            Value left = interpret_expression(e.Binary->left, context);    \
            Value right = interpret_expression(e.Binary->right, context);  \
            Value value = {0};                                             \
            value.type = e.expr->datatype;                                 \
            switch (e.expr->datatype.kind) {                               \
                cases                                                      \
                default: break;                                            \
            }                                                              \
            return value;                                                  \
        }

        case Node_Plus:           binary_expr(numeric(+))
        case Node_Minus:          binary_expr(numeric(-))
        case Node_Mul:            binary_expr(numeric(*))
        case Node_Div:            binary_expr(numeric(/))
        case Node_Mod:            binary_expr(integer(%))
        case Node_Less:           binary_expr(numeric(<))
        case Node_Greater:        binary_expr(numeric(>))
        case Node_LessEquals:     binary_expr(numeric(<=))
        case Node_GreaterEquals:  binary_expr(numeric(>=))
        case Node_Equals:         binary_expr(numeric(==))
        case Node_NotEquals:      binary_expr(numeric(!=))
        case Node_BooleanAnd:     binary_expr(numeric(&&)) // TODO: boolean and & or change control-flow, the right-hand side of the expression should not be interpreted in certain cases
        case Node_BooleanOr:      binary_expr(numeric(||))
        case Node_Bitwise_And:    binary_expr(integer(&))
        case Node_Bitwise_Or:     binary_expr(integer(|))
        case Node_Bitwise_Xor:    binary_expr(integer(^))
        case Node_Bitwise_Lshift: binary_expr(integer(<<))
        case Node_Bitwise_Rshift: binary_expr(integer(>>))

        #undef binary_expr
        #undef integer
        #undef numeric
        #undef C

        case Node_Unary_PreIncrement: break;
        case Node_Unary_PostIncrement: break;
        case Node_Unary_PreDecrement: break;
        case Node_Unary_PostDecrement: break;


        case Node_Unary_Not: {
            Value v = interpret_expression(e.Unary->inner_expr, context);
            v.uint64 = !v.uint64;
            return v;
        }
        case Node_Unary_BitwiseNot: {
            Value v = interpret_expression(e.Unary->inner_expr, context);
            v.uint64 = ~v.uint64;
            return v;
        }
        case Node_Unary_AddressOf: break;
        case Node_Unary_ValueOf: break;
        case Node_Unary_Negate: {
            Value v = interpret_expression(e.Unary->inner_expr, context);
            v.uint64 = -v.uint64; // TODO: this is wrong for ints other than 64bits
            return v;
        }


        case Node_Literal_Integer: return (Value) { .type = type_ambiguousInteger, .uint64 = e.Literal->data.integer };
        case Node_Literal_Decimal: return (Value) { .type = type_ambiguousDecimal, .float64 = e.Literal->data.decimal };
        case Node_Literal_Char:    return (Value) { .type = type_char, .character = e.Literal->data.character };
        case Node_Literal_String:  return (Value) { .type = type_charPointer, .pointer = get_string(e.Literal->data.string) };
        case Node_Literal_True:    return (Value) { .type = type_bool, .uint64 = true };
        case Node_Literal_False:   return (Value) { .type = type_bool, .uint64 = false };
        case Node_Literal_Null:    return (Value) { .type = type_voidPointer, .pointer = null };

        case Node_Variable: {
            if (e.Variable->ref.node == null) {
                ReplVariable* var = scope_context_get(context, e.Variable->name);
                if (var) return var->value;

                printf("Could not find reference\n");
            }

            

        } break;
        case Node_Alloc: {
            u64 size = datatype_bytesize(e.Alloc->type->solvedstate);
            Value v = {0};
            v.pointer = malloc(size);
            v.type = e.expr->datatype;
            return v;
        }
        case Node_Ternary: {
            Value c = interpret_expression(e.Ternary->condition, context);
            if (c.pointer) return interpret_expression(e.Ternary->then_expr, context);
            return interpret_expression(e.Ternary->else_expr, context);
        }
        case Node_ProcCall: {
            return interpret_procedure(e.ProcCall, context);
        }
        case Node_Deref: break;
        case Node_Indexing: break;
        case Node_Cast: break;
        case Node_Sizeof: return (Value) { .type = type_ambiguousInteger, .uint64 = datatype_bytesize(e.Sizeof->type->solvedstate) };
        case Node_Parenthesized: return interpret_expression(e.Parenthesized->inner_expr, context);
        case Node_Compound: break;
    }

    return value_invalid;
}


static Value interpret_statement(NodeRef e, ReplScope* context) {
    switch (e.node->kind) {
        case Node_Declaration: {
            Value v = {0};
            if (e.Declaration->expr.node) v = interpret_expression(e.Declaration->expr, context);
            scope_context_declare(context, e.Declaration->name, v);
        } break;
        case Node_Constant: break;
        case Node_Typedef: break;
        case Node_Procedure: break;
        case Node_Argument: break;
        case Node_Struct: break;
        case Node_Enum: break;
        case Node_EnumEntry: break;
        case Node_Assignment: {
        } break;
        case Node_Scope: {
            ReplScope* inner_scope = push_scope(context);
            foreach (sta, e.Scope->statements) {
                interpret_statement(*sta, inner_scope);
            }
            pop_scope(inner_scope);
        } break;
        case Node_IfStmt: {
            Value v = interpret_expression(e.IfStmt->condition, context);
            if (v.pointer) interpret_statement(e.IfStmt->then_statement, context);
            else interpret_statement(e.IfStmt->else_statement, context);
        } break;
        case Node_WhileStmt: break;
        case Node_ForStmt: break;
        case Node_SwitchStmt: break;
        case Node_ContinueStmt: break;
        case Node_BreakStmt: break;
        case Node_ReturnStmt: break;
        case Node_GotoStmt: break;
        case Node_LabelStmt: break;
        case Node_CaseLabelStmt: break;
        case Node_DefaultLabelStmt: break;
    }

    return (Value) { .type = type_invalid, .pointer = null };
}

static Value interpret_node(NodeRef p, ReplScope* scope) {
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
    NodeRef node = expect_node(repl->parser);

    foreach (var, repl->parser->unresolved_variables) {
        (*var)->ref = get_global_symbol_from_codebase((*var)->name, repl->codebase);
        // if ((*var)->ref == null) printf("unresolved var\n");
    }

    validate_node(repl->parser, node);
    Value v = interpret_node(node, repl->context);
    print_value(v);
    printf("\n");

    return v.uint64;
}