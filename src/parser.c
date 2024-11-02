
static NodeRef expect_expr(Parser* parser);
static NodeRef expect_node(Parser* parser);

static Scope* expectScope(Parser* parser);
static void validate(Parser* parser, Codebase* cb);


static Token expect(Parser* parser, TokenType type) {
    Token token = advance(parser);
    if (token.type == type) return token;
    fatal_parse_error(parser, "Unexpected token type %s. Expected: %s.", TokenType_Names[token.type], TokenType_Names[type]);
}

static Identifier identifier(Parser* parser) {
    Token token = advance(parser);
    if (token.type == Tok_Word) return token.data.string;
    fatal_parse_error(parser, "Invalid identifier: %s", TokenType_Names[token.type]);
}


static inline bool tok(Parser* parser, TokenType type) {
    if (peek(parser).type == type) {
        advance(parser);
        return true;
    }
    return false;
}

// asserts the existence of a semicolon
static inline void semicolon(Parser* parser) {
    if (tok(parser, Tok_Semicolon)) return;
    if (parser->allow_omitting_semicolon) return;
    error_token(parser, "Expected semicolon.");
}

static Token* anyof(Parser* parser, u32 count, ...) {
    va_list args;
    va_start(args, count);
    for (u32 i = 0; i < count; i++) {
        TokenType type = va_arg(args, TokenType);
        Token* tok = &parser->tokens[parser->token_index];
        if (tok->type == type) {
            advance(parser);
            va_end(args);
            return tok;
        }
    }
    va_end(args);
    return null;
}


static Type* type_modifier(Parser* parser, Type* type);

static Type* parseTypeNode(Parser* parser) {
    Type* type = alloc_node(parser, Node_Type_Basic).Type; // TODO: very important: we are allocing Type_Basic but this may be overwriten, not a problem now, but may be problem later

    if (tok(parser, Tok_Keyword_Let)) {
        type->kind = Node_Type_MustInfer;
        return type;
    }

    type->name = identifier(parser);

    Type* mod = type;
    while ( (mod = type_modifier(parser, mod)) ) type = mod;

    return type;
}

static Type* type_modifier(Parser* parser, Type* type) {
    if (tok(parser, Tok_OpenParen)) {
        Type* mod = alloc_node(parser, Node_Type_Procedure).Type;
        mod->procedure.return_type = type;
        mod->solvedstate.numPointers = 1;
        if (tok(parser, Tok_CloseParen)) return mod;

        Type* arg = parseTypeNode(parser);
        mod->procedure.first_argument = arg;
        while (tok(parser, Tok_Comma)) {
            arg->next = parseTypeNode(parser);
            arg = arg->next;
        }
        expect(parser, Tok_CloseParen);
        return mod;
    }

    if (tok(parser, Tok_Mul)) {
        type->solvedstate.numPointers++;
        return type;
    }

    if (tok(parser, Tok_OpenSquare)) {
        Type* mod = alloc_node(parser, Node_Type_Array).Type;
        mod->array.element_type = type;

        if (tok(parser, Tok_CloseSquare)) return mod;

        if (tok(parser, Tok_Dotdot)) {
            mod->kind = Node_Type_Dynamic_Array;
        } else {
            mod->kind = Node_Type_Fixed_Array;
            mod->array.size_expr = expect_expr(parser);
        }

        expect(parser, Tok_CloseSquare);
        return mod;
    }

    return null;
}

static void print_typenode(Type* type) {
    switch (type->kind) {
        case Node_Type_MustInfer: printf("let"); break;
        case Node_Type_Basic: {
            printf("%s", get_string(type->name));
        } break;

        case Node_Type_Procedure: {
            print_typenode(type->procedure.return_type);
            printf("(");
            Type* arg = type->procedure.first_argument;
            if (arg) {
                print_typenode(arg);
                arg = arg->next;
                while (arg) {
                    printf(", ");
                    print_typenode(arg);
                    arg = arg->next;
                }
            }
            printf(")");
        } break;

        case Node_Type_Array: {
            print_typenode(type->array.element_type);
            printf("[]");
        } break;
        case Node_Type_Fixed_Array: {
            print_typenode(type->array.element_type);
            printf("[%d]", 0); // TODO: print proper size
        } break;
        case Node_Type_Dynamic_Array: {
            print_typenode(type->array.element_type);
            printf("[..]");
        } break;

        default: abort();
    }

    printf("%.*s", type->solvedstate.numPointers, "***********");
}


static void resolve_typenode(Parser* parser, Type* type, Codebase* codebase) {

    Datatype datatype = type_invalid;

    switch (type->kind) {

        case Node_Type_Basic: {

            if (codebase) {
                datatype = get_global_type(type->name, codebase);
                break;
            }

            if (type->name == builtin_string_int8)    { datatype = type_int8; break; }
            if (type->name == builtin_string_uint8)   { datatype = type_uint8; break; }
            if (type->name == builtin_string_int16)   { datatype = type_int16; break; }
            if (type->name == builtin_string_uint16)  { datatype = type_uint16; break; }
            if (type->name == builtin_string_int32)   { datatype = type_int32; break; }
            if (type->name == builtin_string_uint32)  { datatype = type_uint32; break; }
            if (type->name == builtin_string_int64)   { datatype = type_int64; break; }
            if (type->name == builtin_string_uint64)  { datatype = type_uint64; break; }
            if (type->name == builtin_string_float32) { datatype = type_float32; break; }
            if (type->name == builtin_string_float64) { datatype = type_float64; break; }
            if (type->name == builtin_string_char)    { datatype = type_char; break; }
            if (type->name == builtin_string_void)    { datatype = type_void; break; }
            if (type->name == builtin_string_string)  { datatype = type_string; break; }

            datatype = get_local_type(parser, type->name);
        } break;

        case Node_Type_Procedure: {

            resolve_typenode(parser, type->procedure.return_type, codebase);

            Type* arg = type->procedure.first_argument;
            while (arg) {
                resolve_typenode(parser, arg, codebase);
                arg = arg->next;
            }

            datatype.kind = Typekind_Procedure;
            datatype.proc_ptr_typenode = type;
        } break;

        case Node_Type_Array: {
            resolve_typenode(parser, type->array.element_type, codebase);
            datatype.kind = Typekind_Array;
            datatype.array_typenode = type;
        } break;

        case Node_Type_Fixed_Array: {
            resolve_typenode(parser, type->array.element_type, codebase);
            datatype.kind = Typekind_Fixed_Array;
            datatype.array_typenode = type;
        } break;

        case Node_Type_Dynamic_Array: {
            resolve_typenode(parser, type->array.element_type, codebase);
            datatype.kind = Typekind_Dynamic_Array;
            datatype.array_typenode = type;
        } break;

        case Node_Type_MustInfer:
            printf("MustInfer\n");
        default:
            datatype = type_invalid;
    }

    if (datatype.kind == Typekind_Invalid) {
        list_add(parser->unresolved_types, type);
    } else {
        u32 np = type->solvedstate.numPointers;
        type->solvedstate = datatype;
        type->solvedstate.numPointers = np;
    }
}

static Type* expectType(Parser* parser) {
    Type* type = parseTypeNode(parser);

    if (type->kind == Node_Type_MustInfer) return type;

    resolve_typenode(parser, type, null);

    return type;
}

static void init_typenode_for_proc(Procedure* proc) {
    Type* type = malloc(sizeof(Type)); // TODO: malloc to remove here
    *type = (Type){0};
    type->kind = Node_Type_Procedure;
    type->loc = proc->loc;
    type->procedure.return_type = proc->return_type;
    type->solvedstate.proc_ptr_typenode = type;

    if (proc->arguments) {
        Type* type_arg = proc->arguments[0].type;
        type->procedure.first_argument = type_arg;
        u32 arg_count = list_length(proc->arguments);
        for (u32 i = 1; i < arg_count; i++) {
            type_arg->next = proc->arguments[i].type;
            type_arg = type_arg->next;
        }
    }

    proc->type_node = type;
}


static bool isBasicType(Parser* parser);
static bool _isBasicType_validModifier(Parser* parser) {

    u32 ti = parser->token_index;

    if (tok(parser, Tok_Mul)) return true;

    if (tok(parser, Tok_OpenParen)) {
        if (tok(parser, Tok_CloseParen)) return true;

        do {
            if (!isBasicType(parser)) goto nope;
        } while (tok(parser, Tok_Comma));

        if (tok(parser, Tok_CloseParen)) return true;
        else goto nope;
    }

    if (tok(parser, Tok_OpenSquare)) {
        if (tok(parser, Tok_CloseSquare)) return true;
        if (tok(parser, Tok_Dotdot) && tok(parser, Tok_CloseSquare)) return true;
        if (tok(parser, Tok_Integer) && tok(parser, Tok_CloseSquare)) return true;
        if (tok(parser, Tok_Word) && tok(parser, Tok_CloseSquare)) return true;
    }

    nope:
    parser->token_index = ti;
    return false;
}

static bool isBasicType(Parser* parser) {
    if (tok(parser, Tok_Word)) {
        if (tok(parser, Tok_Period)) if (!tok(parser, Tok_Word)) return false;
        while (_isBasicType_validModifier(parser));
        return true;
    }
    return false;
}

static u32 peekType(Parser* parser) {

    if (peek(parser).type == Tok_Keyword_Let) return parser->token_index + 1;

    u32 ti = parser->token_index;
    u32 res = 0;
    if (isBasicType(parser)) res = parser->token_index;
    parser->token_index = ti;
    return res;
}

static NodeRef expectEnum(Parser* parser) {
    Enum* en = alloc_node(parser, Node_Enum).Enum;
    en->entries = list_create(EnumEntry);

    advance(parser);
    en->name = identifier(parser);

    expect(parser, Tok_OpenCurl);

    while (!tok(parser, Tok_CloseCurl)) {
        EnumEntry entry = {0};
        entry.loc = get_code_location_here(parser);
        entry.kind = Node_EnumEntry;
        entry._enum = en;
        entry.name = identifier(parser);
        if (tok(parser, Tok_Assign)) entry.expr = expect_expr(parser);
        semicolon(parser);

        list_add(en->entries, entry);
    }

    return (NodeRef)en;
}

static NodeRef expectStruct(Parser* parser) {
    Struct* stru = alloc_node(parser, Node_Struct).Struct;
    stru->fields = list_create(Declaration);

    advance(parser);
    stru->name = identifier(parser);
    expect(parser, Tok_OpenCurl);

    do {
        Declaration field;
        field.loc = get_code_location_here(parser);
        field.kind = Node_Declaration;

        field.include_context = tok(parser, Tok_Keyword_With);
        field.type = expectType(parser);
        field.name = identifier(parser);
        list_add(stru->fields, field);

        while (tok(parser, Tok_Comma)) {
            field.loc.line = peek(parser).line;
            field.name = identifier(parser);
            list_add(stru->fields, field);
        }

        semicolon(parser);
    } while (peek(parser).type != Tok_CloseCurl);

    advance(parser);

    return (NodeRef)stru;
}

static NodeRef expectTypedef(Parser* parser) {
    Typedef* def = alloc_node(parser, Node_Typedef).Typedef;
    advance(parser);
    def->name = identifier(parser);
    if (tok(parser, Tok_Assign)) def->type = expectType(parser);
    semicolon(parser);
    return (NodeRef)def;
}

static NodeRef expectConst(Parser* parser) {
    Constant* co = alloc_node(parser, Node_Constant).Constant;
    advance(parser);
    co->name = identifier(parser);
    expect(parser, Tok_Assign);
    co->expr = expect_expr(parser);
    semicolon(parser);
    return (NodeRef)co;
}

static Argument* expectArgument(Parser* parser) {

    if (tok(parser, Tok_CloseParen))  return null;

    Argument* res = list_create(Argument);
    Argument arg;
    arg.kind = Node_Argument;
    do {
        tok(parser, Tok_Keyword_With);
        arg.loc = get_code_location_here(parser);
        if (parser->tokens[peekType(parser)].type == Tok_Word) arg.type = expectType(parser);
        arg.name = identifier(parser);
        list_add(res, arg);
    } while (tok(parser, Tok_Comma));
    expect(parser, Tok_CloseParen);

    return res;
}

static NodeRef proc_or_var(Parser* parser, bool declare_localy) {

    CodeLocation loc = get_code_location_here(parser);
    Type* type = expectType(parser);
    Identifier name = identifier(parser);

    if (tok(parser, Tok_OpenParen)) {

        Procedure* proc = alloc_node(parser, Node_Procedure).Procedure;
        proc->loc = loc;
        proc->return_type = type;
        proc->name = name;
        proc->arguments = expectArgument(parser);

        proc->operator = tok(parser, Tok_Keyword_As) ? get_binary_node_from_token(advance(parser).type) : Node_Invalid;

        if (declare_localy) declare_symbol(parser, (NodeRef)proc);

        if (!tok(parser, Tok_Semicolon)) {
            u32 stack_ptr = get_symbol_stack_length(parser);
            if (proc->arguments) {
                foreach (arg, proc->arguments) {
                    declare_symbol(parser, (NodeRef)arg);
                }
            }

            proc->sub_node = expect_node(parser);
            set_symbol_stack_length(parser, stack_ptr);
        }

        return (NodeRef)proc;
    }


    Declaration* decl = alloc_node(parser, Node_Declaration).Declaration;
    decl->loc = loc;
    decl->type = type;
    decl->name = name;

    if (tok(parser, Tok_Assign)) decl->expr = expect_expr(parser);

    semicolon(parser);
    return (NodeRef)decl;
}


// static void skipBody(Parser* parser) {
//     while (true) {
//         switch (peek(parser).type) {
//             case Tok_OpenCurl: advance(parser); skipBody(parser); break;
//             case Tok_CloseCurl: advance(parser); return;
//             case Tok_EOF: error_temp("Unmatched curly-bracket.\n"); return;
//             default: advance(parser); break;
//         }
//     }
// }


static void parse_unit(Parser* parser) {
    Unit* unit = list_add_empty(parser->units);
    parser->current_unit = unit;

    unit->arena = arena_create();
    list_init(unit->top_level_nodes);
    list_init(unit->external_symbols);
    list_init(unit->external_types);
    list_init(unit->included_files);

    reset_parser(parser);

    while (peek(parser).type != Tok_EOF) {
        list_clear(parser->local_symbols);
        NodeRef ref = expect_node(parser);
        if (ref.node) list_add(unit->top_level_nodes, ref);
    }

    foreach (item, parser->unresolved_variables) {
        VariableExpression* var = *item;
        var->ref = get_global_symbol(var->name, unit);

        if (!var->ref.node) list_add(unit->external_symbols, var);
    }

    Identifier* includes = unit->included_files;
    foreach (include, includes) {
        char* file_name = alloc_string_copy(get_string(*include)); // TODO: temporary fix. buffer of string returned from get_string() may be realloced
        printf("NOW including: \"%s\"\n", file_name);
        parser_parse_file(parser, file_name);
    }
}







void parser_parse_source(Parser* parser, char* source) {
    lex(parser, source);
    if (setjmp(parser->jump_location) == 0) parse_unit(parser);
}

void parser_parse_file(Parser* parser, char* file_name) {
    u32 content_len = 0;
    char* content = fileread(file_name, &content_len);
    parser->current_file_name = file_name;
    parser_parse_source(parser, content);
    free(content);
}

// must be possible to rebind codebase several times, because units may change
void bind_units(Parser* parser, Codebase* cb) {
    parser->codebase = cb;

    // construct Codebase that contains lists of all procs/types/globals
    #define ensure_list(list) if (list) list_clear(list); else list = list_create(typeof(*list))
    ensure_list(cb->procedures);
    ensure_list(cb->operators);
    ensure_list(cb->global_vars);
    ensure_list(cb->global_consts);
    ensure_list(cb->structs);
    ensure_list(cb->enums);
    ensure_list(cb->type_defs);
    #undef ensure_list

    foreach (unit, parser->units) {
        foreach (top_lvl_sta, unit->top_level_nodes) {
            NodeRef ref = (NodeRef)(*top_lvl_sta);

            switch (ref.node->kind) {
                case Node_Procedure: {
                    list_add(cb->procedures, ref);
                    if (ref.Procedure->operator) list_add(cb->operators, ref);
                } break;
                case Node_Constant:    list_add(cb->global_consts, ref); break;
                case Node_Declaration: list_add(cb->global_vars,   ref); break;
                case Node_Struct:      list_add(cb->structs,       ref); break;
                case Node_Enum:        list_add(cb->enums,         ref); break;
                case Node_Typedef:     list_add(cb->type_defs,     ref); break;

                default: break;
            }
        }
    }

    // resolve types
    foreach (tp, parser->unresolved_types) {
        Type* type = *tp;
        resolve_typenode(parser, type, cb);
        if (type->solvedstate.kind == Typekind_Invalid) {
            // TODO: include type in error message
            error_node(parser, (NodeRef)type, "Failed to resolve type");
            // printf("    "); print_typenode(type); printf("\n");
        }
    }

    // resolve external symbols
    u32 units_length = list_length(parser->units);
    for (u32 i = 0; i < units_length; i++) {
        Unit* u1 = &parser->units[i];
        for (u32 j = 0; j < units_length; j++) {
            Unit* u2 = &parser->units[j];
            if (u1 == u2) continue;

            foreach (item, u1->external_symbols) {
                VariableExpression* var = *item;
                if (var->ref.node) continue; // TODO: why am I doing this? figure out and leave a comment
                var->ref = get_global_symbol(var->name, u2);
            }
        }
    }

    for (u32 i = 0; i < units_length; i++) {
        Unit* u = &parser->units[i];

        foreach (item, u->external_symbols) {
            VariableExpression* var = *item;
            if (node_is_null(var->ref)) error_node(parser, (NodeRef)var, "Undeclared symbol \"%s\".", get_string(var->name));
        }
    }

    { // overloads

        u32 procs_length = list_length(cb->procedures);

        // clear (in case this is rebinding a previously bound codebase)
        // for (u32 i = 0; i < procs_length; i++) {
        //     Procedure* p = cb->procedures[i];
        //     p->next_overload = null;
        //     p->overload = 0;
        // }


        for (u32 i = 0; i < procs_length; i++) {
            Procedure* p1 = cb->procedures[i];

            init_typenode_for_proc(p1);

            if (p1->overload) continue;

            u32 overload_count = 1;
            Procedure* it = p1;

            // for each subsequent procedure
            for (u32 j = i + 1; j < procs_length; j++) {
                Procedure* p2 = cb->procedures[j];
                if (it->name != p2->name) continue;

                p2->overload = ++overload_count;
                it->next_overload = p2;
                it = p2;

                // TODO: check if it is a valid overload pair
            }

            u32 builtin_procs_len = list_length(builtin_procedures);
            for (u32 i = 0; i < builtin_procs_len; i++) {
                Procedure* p2 = &builtin_procedures[i];
                if (it->name != p2->name) continue;

                p2->overload = ++overload_count;
                it->next_overload = p2;
                it = p2;
            }

            if (overload_count != 1) {
                p1->overload = 1;
                it->next_overload = p1; // to make it a cyclic linked list
            }
        }
    }
}

static void print_codebase(Codebase* cb) {
    printf("Globals:\n");
    foreach (g, cb->global_vars) {
        printf("    %d. %s\n", g_index, get_string((*g)->name));
    }
}

Codebase parse(Parser* parser) {

    Codebase cb = {0};
    bind_units(parser, &cb);


    // TODO: we may want to run the validator even if the parser made errors,
    // its just that we dont want to validate if there was a syntax error, but parser may produce errors that are not syntax errors
    u32 num_errors = list_length(parser->errors);
    if (num_errors) {
        print_errors(parser);
        printf("There were %d errors during parsing.\n", num_errors);
        return cb;
    }

    printf("Validate...\n");
    validate(parser, &cb);
    num_errors = list_length(parser->errors);
    if (num_errors) {
        print_errors(parser);
        printf("There were %d errors during validation.\n", num_errors);
        return cb;
    }

    // print_codebase(&cb);

    return cb;
}
