

static Scope* expectScope();
static Expression* parseExpression();
static Expression* expectExpression();
static Statement* expectStatement();
static void validate(Codebase* cb);

static inline Token peek() {
    return tokens[token_index];
}

static inline Token peek_at(i32 offset) {
    return tokens[token_index + offset];
}

// asserts the existence of a semicolon
static inline void semicolon() {
    if (tokens[token_index].type != Tok_Semicolon) {
        error_token("Expected semicolon.");
        return;
    }
    token_index++;
}

static Identifier identifier() {
    Token token = tokens[token_index];
    if (token.type != Tok_Word) {
        fatal_parse_error("Invalid identifier: %s", TokenType_Names[tokens[token_index].type]);
        return -1;
    }
    token_index++;
    return token.data.string;
}

static void expect(TokenType type) {
    if (tokens[token_index].type != type) {
        fatal_parse_error("Unexpected token type %s.", TokenType_Names[tokens[token_index].type]);
        return;
    }
    token_index++;
}

static void unexpectedToken() {
    fatal_parse_error("Unexpected token.");
}

static inline bool tok(TokenType type) {
    if (tokens[token_index].type == type) {
        token_index++;
        return true;
    }
    return false;
}

static Token* anyof(u32 count, ...) {
    va_list args;
    va_start(args, count);
    for (u32 i = 0; i < count; i++) {
        TokenType type = va_arg(args, TokenType);
        Token* tok = &tokens[token_index];
        if (tok->type == type) {
            token_index++;
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

static Type* newTypeNode(TypeNode node_type) {
    Type* res = calloc(1, sizeof(Type)); // TODO: use arena allocator
    res->nodebase = node_init();
    res->node_type = node_type;
    return res;
}

static Type* type_modifier(Type* type);

static Type* parseTypeNode() {
    Type* type = newTypeNode(TypeNode_Normal);

    if (tok(Tok_Keyword_Let)) {
        type->node_type = TypeNode_MustInfer;
        return type;
    }

    type->name = identifier();

    Type* mod = type;
    while ( (mod = type_modifier(mod)) ) type = mod;

    return type;
}

static Type* type_modifier(Type* type) {
    if (tok(Tok_OpenParen)) {
        Type* mod = newTypeNode(TypeNode_Procedure);
        mod->procedure.return_type = type;
        mod->solvedstate.numPointers = 1;
        if (tok(Tok_CloseParen)) return mod;

        Type* arg = parseTypeNode();
        mod->procedure.first_argument = arg;
        while (tok(Tok_Comma)) {
            arg->next = parseTypeNode();
            arg = arg->next;
        }
        expect(Tok_CloseParen);
        return mod;
    }

    if (tok(Tok_Mul)) {
        type->solvedstate.numPointers++;
        return type;
    }

    if (tok(Tok_OpenSquare)) {
        Type* mod = newTypeNode(TypeNode_Array);
        mod->array.element_type = type;

        if (tok(Tok_CloseSquare)) return mod;

        if (tok(Tok_Dotdot)) {
            mod->node_type = TypeNode_Dynamic_Array;
        } else {
            mod->node_type = TypeNode_Fixed_Array;
            mod->array.size_expr = expectExpression();
        }

        expect(Tok_CloseSquare);
        return mod;
    }

    return null;
}

static void printType(Type* type) {
    switch (type->node_type) {
        case TypeNode_MustInfer: printf("let"); break;
        case TypeNode_Normal: {
            printf("%s", get_string(type->name));
            printf("%.*s", type->solvedstate.numPointers, "***********");
        } break;

        case TypeNode_Procedure: {
            printType(type->procedure.return_type);
            printf("(");
            Type* arg = type->procedure.first_argument;
            if (arg) {
                printType(arg);
                arg = arg->next;
                while (arg) {
                    printf(", ");
                    printType(arg);
                    arg = arg->next;
                }
            }
            printf(")");
        } break;
    }
}


static void resolve_typenode(Type* type, Codebase* codebase) {

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

            datatype = get_type_from_statement(get_local_type(type->name));
        } break;

        case TypeNode_Procedure: {

            resolve_typenode(type->procedure.return_type, codebase);

            Type* arg = type->procedure.first_argument;
            while (arg) {
                resolve_typenode(arg, codebase);
                arg = arg->next;
            }

            datatype.kind = Typekind_Procedure;
            datatype.proc_ptr_typenode = type;
        } break;

        case TypeNode_Array: {
            resolve_typenode(type->array.element_type, codebase);
            datatype.kind = Typekind_Array;
        } break;

        case TypeNode_MustInfer:
        default: datatype = type_invalid;
    }

    if (datatype.kind == Typekind_Invalid) {
        list_add(parser.unresolved_types, type);
    } else {
        u32 np = type->solvedstate.numPointers;
        type->solvedstate = datatype;
        type->solvedstate.numPointers = np;
    }
}

static Type* expectType() {
    Type* type = parseTypeNode();

    if (type->node_type == TypeNode_MustInfer) return type;

    resolve_typenode(type, null);

    return type;
}

static void init_typenode_for_proc(Procedure* proc) {
    Type* type = newTypeNode(TypeNode_Procedure);
    type->nodebase = proc->base.nodebase;
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

static ProcArg* expectProcArguments() {

    if (tok(Tok_CloseParen)) {
        return null;
    }

    ProcArg* res = list_create(ProcArg);
    ProcArg arg;
    arg.base.statementType = Statement_Argument;
    do {
        tok(Tok_Keyword_With);
        arg.base.nodebase = node_init();
        arg.type = expectType();
        arg.name = identifier();
        list_add(res, arg);
    } while (tok(Tok_Comma));
    expect(Tok_CloseParen);

    return res;
}

static Statement* expectEnum() {

    Enum* en = allocStatement(Statement_Enum);
    en->entries = list_create(EnumEntry);

    token_index++;
    en->name = identifier();

    expect(Tok_OpenCurl);

    while (!tok(Tok_CloseCurl)) {
        EnumEntry entry = {0};
        entry.base.nodebase = node_init();
        entry.base.statementType = Statement_EnumEntry;
        entry._enum = en;
        entry.name = identifier();
        if (tok(Tok_Assign)) entry.expr = expectExpression();
        semicolon();

        list_add(en->entries, entry);
    }

    return (Statement*)en;
}

static Statement* expectStruct() {
    Struct* stru = allocStatement(Statement_Struct);
    stru->fields = list_create(Declaration);

    token_index++;
    stru->name = identifier();
    expect(Tok_OpenCurl);

    do {
        Declaration field;
        field.base.nodebase = node_init();
        field.base.statementType = Statement_Declaration;

        field.include_context = tok(Tok_Keyword_With);
        field.type = expectType();
        field.name = identifier();
        list_add(stru->fields, field);

        while (tok(Tok_Comma)) {
            field.base.nodebase.lineNumber = tokens[token_index].line;
            field.name = identifier();
            list_add(stru->fields, field);
        }

        semicolon();
    } while (tokens[token_index].type != Tok_CloseCurl);

    token_index++;

    return (Statement*)stru;
}

static Statement* expectTypedef() {
    Typedef* def = allocStatement(Statement_Typedef);
    token_index++;
    def->name = identifier();
    if (tok(Tok_Assign)) def->type = expectType();
    semicolon();
    return (Statement*)def;
}

static Statement* expectConst() {
    Declaration* decl = allocStatement(Statement_Constant);
    token_index++;
    decl->name = identifier();
    expect(Tok_Assign);
    decl->expr = expectExpression();
    semicolon();
    return (Statement*)decl;
}


static Statement* proc_or_var(bool declare_localy) {

    Node node = node_init();
    Type* type = expectType();
    Identifier name = identifier();

    if (tok(Tok_OpenParen)) {

        Procedure* proc = allocStatement(Statement_Procedure);
        proc->base.nodebase = node;
        proc->returnType = type;
        proc->name = name;
        proc->arguments = expectProcArguments();

        if (declare_localy) declare_symbol((Statement*)proc);

        if (tok(Tok_Semicolon)) {
            proc->scope = null;
        } else {

            u32 num_args = 0;
            if (proc->arguments) {
                foreach (arg, proc->arguments) {
                    declare_symbol((Statement*)arg);
                }
                num_args = arg_count;
            }

            proc->scope = expectScope();
            stack_pop(num_args);
        }

        return (Statement*)proc;
    }


    Declaration* decl = allocStatement(Statement_Declaration);
    decl->base.nodebase = node;
    decl->type = type;
    decl->name = name;

    if (tok(Tok_OpenSquare)) {
        decl->base.statementType = Statement_FixedArray;
        decl->expr = expectExpression();
        expect(Tok_CloseSquare);
    } else if (tok(Tok_Assign)) {
        decl->expr = expectExpression();
    }

    semicolon();
    return (Statement*)decl;
}

#include "parser_expressions.c"
#include "parser_statements.c"


static void skipBody() {
    while (true) {
        switch (tokens[token_index].type) {
            case Tok_OpenCurl: token_index++; skipBody(); break;
            case Tok_CloseCurl: token_index++; return;
            case Tok_EOF: error_temp("Unmatched curly-bracket.\n"); return;
            default: token_index++; break;
        }
    }
}

static Statement* parse_top_level_statement() {

    switch (tokens[token_index].type) {

        case Tok_Keyword_Include: {
            token_index++;
            expect(Tok_String);
            Identifier string = tokens[token_index - 1].data.string;
            // printf("include \"%s\"\n", get_string(string));
            addFile(get_string(string), null);
            semicolon();
        } break;

        case Tok_Keyword_Struct: return expectStruct();
        case Tok_Keyword_Enum: return expectEnum();
        case Tok_Keyword_Type: return expectTypedef();
        case Tok_Keyword_Const: return expectConst();

        case Tok_Keyword_Let:
        case Tok_Word: return proc_or_var(false);

        case Tok_EOF: return null;

        default: {
            unexpectedToken();
        } break;
    }

    return null;
}

static Unit parse_unit() {
    Unit unit = {0};
    current_unit = &unit;

    unit.arena = arena_create();
    unit.top_level_statements = list_create(Statement*);
    unit.external_symbols = list_create(VariableExpression*);
    unit.external_types = list_create(Type*);

    list_clear(parser.unresolved_variables);

    token_index = 0;
    while (peek().type != Tok_EOF) {
        list_clear(parser.local_symbols);
        Statement* top_lvl_sta = parse_top_level_statement();
        if (top_lvl_sta) list_add(unit.top_level_statements, top_lvl_sta);
    }

    foreach (item, parser.unresolved_variables) {
        VariableExpression* var = *item;
        var->ref = get_global_symbol(var->name, current_unit);

        if (!var->ref) list_add(current_unit->external_symbols, var);
    }

    // {
    //     printf("Unresolved: (external symbols)\n");
    //     foreach (item, current_unit->external_symbols) {
    //         VariableExpression* var = *item;
    //         printf("    \"%s\" at line %d\n", get_string(var->name), var->base.nodebase.lineNumber);
    //     }
    // }


    return unit;
}

static Unit parse_file(char* file_name) {
    u32 content_len = 0;
    char* content = fileread(file_name, &content_len);
    lex(content);
    free(content);

    Unit u = parse_unit();
    return u;
}


// must be possible to rebind codebase several times, because units may change
void bind_units(Unit* units, Codebase* cb) {

    // construct Codebase that contains lists of all procs/types/globals
    #define ensure_list(list) if (list) list_clear(list); else list = list_create(typeof(*list))
    ensure_list(cb->procedures);
    ensure_list(cb->global_vars);
    ensure_list(cb->global_consts);
    ensure_list(cb->structs);
    ensure_list(cb->enums);
    ensure_list(cb->type_defs);
    #undef ensure_list

    foreach (unit, units) {
        foreach (top_lvl_sta, unit->top_level_statements) {
            Statement* sta = *top_lvl_sta;

            switch (sta->statementType) {
                case Statement_Procedure: list_add(cb->procedures, sta); break;

                case Statement_Constant: list_add(cb->global_consts, sta); break;
                case Statement_FixedArray:
                case Statement_Declaration: list_add(cb->global_vars, sta); break;
                case Statement_Struct: list_add(cb->structs, sta); break;
                case Statement_Enum: list_add(cb->enums, sta); break;
                case Statement_Typedef: list_add(cb->type_defs, sta); break;

                default: break;
            }
        }
    }

    // resolve types
    foreach (tp, parser.unresolved_types) {
        Type* type = *tp;
        resolve_typenode(type, cb);
        if (type->solvedstate.kind == Typekind_Invalid) {
            error_node(type, "Failed to resolve type");
            printf("    "); printType(type); printf("\n");
        }
    }

    // resolve external symbols
    u32 units_length = list_length(units);
    for (u32 i = 0; i < units_length; i++) {
        Unit* u1 = &units[i];
        for (u32 j = 0; j < units_length; j++) {
            Unit* u2 = &units[j];
            if (u1 == u2) continue;

            foreach (item, u1->external_symbols) {
                VariableExpression* var = *item;
                if (var->ref) continue;
                var->ref = get_global_symbol(var->name, u2);
            }
        }
    }

    for (u32 i = 0; i < units_length; i++) {
        Unit* u = &units[i];

        foreach (item, u->external_symbols) {
            VariableExpression* var = *item;
            if (var->ref == null) error_node(var, "Undeclared symbol \"%s\".", get_string(var->name));
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

            if (overload_count != 1) p1->overload = 1;
        }
    }
}

static void print_codebase(Codebase* cb) {
    printf("Globals:\n");
    foreach (g, cb->global_vars) {
        printf("    %d. %s\n", g_index, get_string((*g)->name));
    }
}

static Codebase parse() {

    Unit* units = list_create(Unit);

    for (u32 i = 0; i < list_length(parser.src_files); i++) {
        File* file = &parser.src_files[i];
        parser.current_file_index = i;

        Unit u = parse_file(file->filename);
        list_add(units, u);
    }

    Codebase cb = {0};
    bind_units(units, &cb);

    list_delete(units);

    // TODO: we may want to run the validator even if the parser made errors,
    // its just that we dont want to validate if there was a syntax error, but parser may produce errors that are not syntax errors
    if (numberOfErrors) {
        printf("There were %d errors during parsing.\n", numberOfErrors);
        exit(0);
    }

    printf("Validate...\n");
    validate(&cb);
    if (numberOfErrors) {
        printf("There were %d errors during validation.\n", numberOfErrors);
        exit(0);
    }

    // print_codebase(&cb);

    return cb;
}
