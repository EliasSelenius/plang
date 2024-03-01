

static Scope* expectScope(Parser* parser);
static Expression* parseExpression(Parser* parser);
static Expression* expectExpression(Parser* parser);
static Statement* expectStatement(Parser* parser);
static void validate(Parser* parser, Codebase* cb);


static Identifier identifier(Parser* parser) {
    Token token = peek(parser);
    if (token.type != Tok_Word) {
        fatal_parse_error(parser, "Invalid identifier: %s", TokenType_Names[token.type]);
        return -1;
    }
    advance(parser);
    return token.data.string;
}

static void expect(Parser* parser, TokenType type) {
    if (advance(parser).type != type) {
        fatal_parse_error(parser, "Unexpected token type %s.", TokenType_Names[peek(parser).type]);
    }
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


static Datatype get_type_from_statement(Statement* sta) {

    if (sta == null) return type_invalid;
    if (sta->statementType == Statement_Struct) return (Datatype) { .kind = Typekind_Struct, .data_ptr = sta };
    if (sta->statementType == Statement_Enum) return (Datatype) { .kind = Typekind_Enum, .data_ptr = sta };
    if (sta->statementType == Statement_Typedef) return (Datatype) { .kind = Typekind_Typedef, .data_ptr = sta };

    return type_invalid;
}


static Type* newTypeNode_(TypeNode node_type, Node nodebase) {
    Type* res = calloc(1, sizeof(Type)); // TODO: use arena allocator
    res->nodebase = nodebase;
    res->node_type = node_type;
    return res;
}
static Type* newTypeNode(Parser* parser, TypeNode node_type) { return newTypeNode_(node_type, node_init(parser)); }

static Type* type_modifier(Parser* parser, Type* type);

static Type* parseTypeNode(Parser* parser) {
    Type* type = newTypeNode(parser, TypeNode_Normal);

    if (tok(parser, Tok_Keyword_Let)) {
        type->node_type = TypeNode_MustInfer;
        return type;
    }

    type->name = identifier(parser);

    Type* mod = type;
    while ( (mod = type_modifier(parser, mod)) ) type = mod;

    return type;
}

static Type* type_modifier(Parser* parser, Type* type) {
    if (tok(parser, Tok_OpenParen)) {
        Type* mod = newTypeNode(parser, TypeNode_Procedure);
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
        Type* mod = newTypeNode(parser, TypeNode_Array);
        mod->array.element_type = type;

        if (tok(parser, Tok_CloseSquare)) return mod;

        if (tok(parser, Tok_Dotdot)) {
            mod->node_type = TypeNode_Dynamic_Array;
        } else {
            mod->node_type = TypeNode_Fixed_Array;
            mod->array.size_expr = expectExpression(parser);
        }

        expect(parser, Tok_CloseSquare);
        return mod;
    }

    return null;
}

static void print_typenode(Type* type) {
    switch (type->node_type) {
        case TypeNode_MustInfer: printf("let"); break;
        case TypeNode_Normal: {
            printf("%s", get_string(type->name));
        } break;

        case TypeNode_Procedure: {
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

        case TypeNode_Array: {
            print_typenode(type->array.element_type);
            printf("[]");
        } break;
        case TypeNode_Fixed_Array: {
            print_typenode(type->array.element_type);
            printf("[%d]", 0); // TODO: print proper size
        } break;
        case TypeNode_Dynamic_Array: {
            print_typenode(type->array.element_type);
            printf("[..]");
        } break;
    }

    printf("%.*s", type->solvedstate.numPointers, "***********");
}


static void resolve_typenode(Parser* parser, Type* type, Codebase* codebase) {

    Datatype datatype = type_invalid;

    switch (type->node_type) {

        case TypeNode_Normal: {

            if (codebase) {
                datatype = get_type_from_statement(get_global_type(type->name, codebase));
                break;
            }

            if (type->name == type_name_int8)    { datatype = type_int8; break; }
            if (type->name == type_name_uint8)   { datatype = type_uint8; break; }
            if (type->name == type_name_int16)   { datatype = type_int16; break; }
            if (type->name == type_name_uint16)  { datatype = type_uint16; break; }
            if (type->name == type_name_int32)   { datatype = type_int32; break; }
            if (type->name == type_name_uint32)  { datatype = type_uint32; break; }
            if (type->name == type_name_int64)   { datatype = type_int64; break; }
            if (type->name == type_name_uint64)  { datatype = type_uint64; break; }
            if (type->name == type_name_float32) { datatype = type_float32; break; }
            if (type->name == type_name_float64) { datatype = type_float64; break; }
            if (type->name == type_name_char)    { datatype = type_char; break; }
            if (type->name == type_name_void)    { datatype = type_void; break; }

            datatype = get_type_from_statement(get_local_type(parser, type->name));
        } break;

        case TypeNode_Procedure: {

            resolve_typenode(parser, type->procedure.return_type, codebase);

            Type* arg = type->procedure.first_argument;
            while (arg) {
                resolve_typenode(parser, arg, codebase);
                arg = arg->next;
            }

            datatype.kind = Typekind_Procedure;
            datatype.proc_ptr_typenode = type;
        } break;

        case TypeNode_Array: {
            resolve_typenode(parser, type->array.element_type, codebase);
            datatype.kind = Typekind_Array;
            datatype.array_typenode = type;
        } break;

        case TypeNode_Fixed_Array: {
            resolve_typenode(parser, type->array.element_type, codebase);
            datatype.kind = Typekind_Fixed_Array;
            datatype.array_typenode = type;
        } break;

        case TypeNode_Dynamic_Array: {
            resolve_typenode(parser, type->array.element_type, codebase);
            datatype.kind = Typekind_Dynamic_Array;
            datatype.array_typenode = type;
        } break;

        case TypeNode_MustInfer:
        default: datatype = type_invalid;
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

    if (type->node_type == TypeNode_MustInfer) return type;

    resolve_typenode(parser, type, null);

    return type;
}

static void init_typenode_for_proc(Procedure* proc) {
    Type* type = newTypeNode_(TypeNode_Procedure, proc->base.nodebase);
    type->procedure.return_type = proc->returnType;
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


static Statement* expectEnum(Parser* parser) {

    Enum* en = allocStatement(parser, Statement_Enum);
    en->entries = list_create(EnumEntry);

    advance(parser);
    en->name = identifier(parser);

    expect(parser, Tok_OpenCurl);

    while (!tok(parser, Tok_CloseCurl)) {
        EnumEntry entry = {0};
        entry.base.nodebase = node_init(parser);
        entry.base.statementType = Statement_EnumEntry;
        entry._enum = en;
        entry.name = identifier(parser);
        if (tok(parser, Tok_Assign)) entry.expr = expectExpression(parser);
        semicolon(parser);

        list_add(en->entries, entry);
    }

    return (Statement*)en;
}

static Statement* expectStruct(Parser* parser) {
    Struct* stru = allocStatement(parser, Statement_Struct);
    stru->fields = list_create(Declaration);

    advance(parser);
    stru->name = identifier(parser);
    expect(parser, Tok_OpenCurl);

    do {
        Declaration field;
        field.base.nodebase = node_init(parser);
        field.base.statementType = Statement_Declaration;

        field.include_context = tok(parser, Tok_Keyword_With);
        field.type = expectType(parser);
        field.name = identifier(parser);
        list_add(stru->fields, field);

        while (tok(parser, Tok_Comma)) {
            field.base.nodebase.loc.line = peek(parser).line;
            field.name = identifier(parser);
            list_add(stru->fields, field);
        }

        semicolon(parser);
    } while (peek(parser).type != Tok_CloseCurl);

    advance(parser);

    return (Statement*)stru;
}

static Statement* expectTypedef(Parser* parser) {
    Typedef* def = allocStatement(parser, Statement_Typedef);
    advance(parser);
    def->name = identifier(parser);
    if (tok(parser, Tok_Assign)) def->type = expectType(parser);
    semicolon(parser);
    return (Statement*)def;
}

static Statement* expectConst(Parser* parser) {
    Declaration* decl = allocStatement(parser, Statement_Constant);
    advance(parser);
    decl->name = identifier(parser);
    expect(parser, Tok_Assign);
    decl->expr = expectExpression(parser);
    semicolon(parser);
    return (Statement*)decl;
}

static ProcArg* expectProcArguments(Parser* parser) {

    if (tok(parser, Tok_CloseParen)) {
        return null;
    }

    ProcArg* res = list_create(ProcArg);
    ProcArg arg;
    arg.base.statementType = Statement_Argument;
    do {
        tok(parser, Tok_Keyword_With);
        arg.base.nodebase = node_init(parser);
        arg.type = expectType(parser);
        arg.name = identifier(parser);
        list_add(res, arg);
    } while (tok(parser, Tok_Comma));
    expect(parser, Tok_CloseParen);

    return res;
}

static Statement* proc_or_var(Parser* parser, bool declare_localy) {

    Node node = node_init(parser);
    Type* type = expectType(parser);
    Identifier name = identifier(parser);

    if (tok(parser, Tok_OpenParen)) {

        Procedure* proc = allocStatement(parser, Statement_Procedure);
        proc->base.nodebase = node;
        proc->returnType = type;
        proc->name = name;
        proc->arguments = expectProcArguments(parser);

        if (declare_localy) declare_symbol(parser, (Statement*)proc);

        if (tok(parser, Tok_Semicolon)) {
            proc->scope = null;
        } else {

            u32 num_args = 0;
            if (proc->arguments) {
                foreach (arg, proc->arguments) {
                    declare_symbol(parser, (Statement*)arg);
                }
                num_args = arg_count;
            }

            proc->scope = expectScope(parser);
            stack_pop(parser, num_args);
        }

        return (Statement*)proc;
    }


    Declaration* decl = allocStatement(parser, Statement_Declaration);
    decl->base.nodebase = node;
    decl->type = type;
    decl->name = name;

    if (tok(parser, Tok_Assign)) decl->expr = expectExpression(parser);

    semicolon(parser);
    return (Statement*)decl;
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

static Statement* parse_top_level_statement(Parser* parser) {

    switch (peek(parser).type) {

        case Tok_Keyword_Include: {
            advance(parser);
            expect(parser, Tok_String);
            Identifier file_name = peek_at(parser, -1).data.string;
            printf("include \"%s\"\n", get_string(file_name));
            list_add(parser->current_unit->included_files, file_name);
            semicolon(parser);
        } break;

        case Tok_Keyword_Struct: return expectStruct(parser);
        case Tok_Keyword_Enum: return expectEnum(parser);
        case Tok_Keyword_Type: return expectTypedef(parser);
        case Tok_Keyword_Const: return expectConst(parser);

        case Tok_Keyword_Let:
        case Tok_Word: return proc_or_var(parser, false);

        case Tok_EOF: return null;

        default: {
            fatal_parse_error(parser, "Unexpected token.");
        } break;
    }

    return null;
}

static void parse_unit(Parser* parser) {
    Unit* unit = list_add_empty(parser->units);
    parser->current_unit = unit;

    unit->arena = arena_create();
    unit->top_level_statements = list_create(Statement*);
    unit->external_symbols = list_create(VariableExpression*);
    unit->external_types = list_create(Type*);
    unit->included_files = list_create(Identifier);

    reset_parser(parser);

    while (peek(parser).type != Tok_EOF) {
        list_clear(parser->local_symbols);
        Statement* top_lvl_sta = parse_top_level_statement(parser);
        if (top_lvl_sta) list_add(unit->top_level_statements, top_lvl_sta);
    }

    foreach (item, parser->unresolved_variables) {
        VariableExpression* var = *item;
        var->ref = get_global_symbol(var->name, unit);

        if (!var->ref) list_add(unit->external_symbols, var);
    }

    Identifier* includes = unit->included_files;
    foreach (include, includes) {
        char* file_name = get_string(*include);
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
    parser_parse_source(parser, content);
    free(content);
}

// must be possible to rebind codebase several times, because units may change
void bind_units(Parser* parser, Codebase* cb) {

    // construct Codebase that contains lists of all procs/types/globals
    #define ensure_list(list) if (list) list_clear(list); else list = list_create(typeof(*list))
    ensure_list(cb->procedures);
    ensure_list(cb->global_vars);
    ensure_list(cb->global_consts);
    ensure_list(cb->structs);
    ensure_list(cb->enums);
    ensure_list(cb->type_defs);
    #undef ensure_list

    foreach (unit, parser->units) {
        foreach (top_lvl_sta, unit->top_level_statements) {
            Statement* sta = *top_lvl_sta;

            switch (sta->statementType) {
                case Statement_Procedure: list_add(cb->procedures, sta); break;

                case Statement_Constant: list_add(cb->global_consts, sta); break;
                case Statement_Declaration: list_add(cb->global_vars, sta); break;
                case Statement_Struct: list_add(cb->structs, sta); break;
                case Statement_Enum: list_add(cb->enums, sta); break;
                case Statement_Typedef: list_add(cb->type_defs, sta); break;

                default: break;
            }
        }
    }

    // resolve types
    foreach (tp, parser->unresolved_types) {
        Type* type = *tp;
        resolve_typenode(parser, type, cb);
        if (type->solvedstate.kind == Typekind_Invalid) {
            error_node(parser, type, "Failed to resolve type");
            printf("    "); print_typenode(type); printf("\n");
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
                if (var->ref) continue; // TODO: why am I doing this? figure out and leave a comment
                var->ref = get_global_symbol(var->name, u2);
            }
        }
    }

    for (u32 i = 0; i < units_length; i++) {
        Unit* u = &parser->units[i];

        foreach (item, u->external_symbols) {
            VariableExpression* var = *item;
            if (var->ref == null) error_node(parser, var, "Undeclared symbol \"%s\".", get_string(var->name));
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
