

static Scope* expectScope();
static Expression* parseExpression();
static Expression* expectExpression();
static Statement* expectStatement();

static inline Token token() {
    return parser.tokens[parser.token_index];
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
    return token.string;
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


static Datatype getType(Namespace* ns, Identifier name) {
    foreach (def, ns->type_defs) {
        if (def->name == name) return (Datatype) { .kind = Typekind_Typedef, .data_ptr = def };
    }

    foreach (stru, ns->structs) {
        if (stru->name == name) return (Datatype) { .kind = Typekind_Struct, .data_ptr = stru };
    }

    foreach (en, ns->enums) {
        if (en->name == name) return (Datatype) { .kind = Typekind_Enum, .data_ptr = en };
    }

    foreach (opaque, ns->opaque_types) {
        if (*opaque == name) {
            Datatype res = { .kind = Typekind_Opaque, .data_ptr = (void*)name };
            return res;
        }
    }

    return type_invalid;
}

static Datatype getLocalType(Identifier name) {
    Statement* sta = get_local_type(name);

    if (sta == null) return type_invalid;
    if (sta->statementType == Statement_Struct) return (Datatype) { .kind = Typekind_Struct, .data_ptr = sta };
    if (sta->statementType == Statement_Enum) return (Datatype) { .kind = Typekind_Enum, .data_ptr = sta };
    if (sta->statementType == Statement_Typedef) return (Datatype) { .kind = Typekind_Typedef, .data_ptr = sta };

    return type_invalid;
}

static Type* newTypeNode(TypeNode node_type) {
    Type* res = calloc(1, sizeof(Type));
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
    if (tok(Tok_Period)) {
        type->namespace_name = type->name;
        type->name = identifier();
    }

    Type* mod = type;
    while ( (mod = type_modifier(mod)) ) type = mod;

    return type;
}

static Type* type_modifier(Type* type) {
    if (tok(Tok_OpenParen)) {
        Type* mod = newTypeNode(TypeNode_Procedure);
        mod->procedure.return_type = type;
        if (tok(Tok_CloseParen)) return mod;

        Type* arg = parseTypeNode();
        mod->procedure.arguments = arg;
        while (tok(Tok_Comma)) {
            arg->next = parseTypeNode();
            arg = arg->next;
        }
        expect(Tok_CloseParen);
        return mod;
    }

    if (tok(Tok_Mul)) {
        Type* mod = newTypeNode(TypeNode_Pointer);
        mod->pointedto = type;
        return mod;
    }

    return null;
}

static void printType(Type* type) {
    switch (type->node_type) {
        case TypeNode_MustInfer: printf("let"); break;
        case TypeNode_Normal: {
            if (type->namespace_name) printf("%s.%s", get_string(type->namespace_name), get_string(type->name));
            else printf("%s", get_string(type->name));
        } break;
        case TypeNode_Pointer: {
            printType(type->pointedto);
            printf("*");
        } break;
        case TypeNode_Procedure: {
            printType(type->procedure.return_type);
            printf("(");
            Type* arg = type->procedure.arguments;
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


static bool typenode2datatype_globalcheck = false;
static Datatype typenode2datatype(Type* type) {
    switch (type->node_type) {
        case TypeNode_MustInfer: return type_invalid;

        case TypeNode_Pointer: {
            Datatype res = typenode2datatype(type->pointedto);
            res.numPointers++;
            return res;
        }

        case TypeNode_Normal: {
            if (type->namespace_name && typenode2datatype_globalcheck) {
                Namespace* ns = getNamespace(type->namespace_name);
                return getType(ns, type->name);
            }

            if (type->name == type_name_int8) return type_int8;
            else if (type->name == type_name_uint8) return type_uint8;
            else if (type->name == type_name_int16) return type_int16;
            else if (type->name == type_name_uint16) return type_uint16;
            else if (type->name == type_name_int32) return type_int32;
            else if (type->name == type_name_uint32) return type_uint32;
            else if (type->name == type_name_int64) return type_int64;
            else if (type->name == type_name_uint64) return type_uint64;
            else if (type->name == type_name_float32) return type_float32;
            else if (type->name == type_name_float64) return type_float64;
            else if (type->name == type_name_char) return type_char;
            else if (type->name == type_name_void) return type_void;

            Datatype res = getLocalType(type->name);
            if (typenode2datatype_globalcheck) {
                if (res.kind == Typekind_Invalid) res = getType(get_file(type->nodebase.file_index)->namespace, type->name);
                if (res.kind == Typekind_Invalid) res = getType(g_Codebase.namespaces[0], type->name);
            }
            return res;
        }

        case TypeNode_Procedure: {

            if (!typenode2datatype_globalcheck) return type_invalid;

            Datatype retType = typenode2datatype(type->procedure.return_type);
            if (retType.kind == Typekind_Invalid) return type_invalid;

            // note that there is a potential memory leak here, but maybe we dont care.
            ProcSignature* sig = createSignature(retType);
            Type* arg = type->procedure.arguments;
            while (arg) {
                addArgument(sig, typenode2datatype(arg), 0);
                arg = arg->next;
            }

            return (Datatype) {
                .kind = Typekind_Procedure,
                .procedure = sig,
                .numPointers = 1
            };
        }
    }
}

static Type* expectType() {
    Type* type = parseTypeNode();

    if (type->node_type == TypeNode_MustInfer) return type;

    type->solvedstate = typenode2datatype(type);
    if (type->solvedstate.kind == Typekind_Invalid) {
        list_add(parser.unresolved_types, type);
    }

    return type;
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

// static bool expectDeclaration(Declaration* decl, Type* type, Identifier name) {

//     decl->base.statementType = Statement_Declaration;

//     decl->type = type;
//     decl->name = name;

//     if (tok(Tok_Assign)) decl->expr = expectExpression();
//     else if (tok(Tok_OpenSquare)) {
//         decl->base.statementType = Statement_FixedArray_Declaration;
//         decl->expr = expectExpression();
//         expect(Tok_CloseSquare);
//     }

//     return tok(Tok_Comma);
// }

// static Declaration expectDeclarationNext(Declaration* decl) {
//     Declaration res = {0};
//     res.
//     return res;
// }

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

static Enum expectEnum() {
    Enum en = {0};
    en.base.nodebase = node_init();
    en.base.statementType = Statement_Enum;
    en.entries = list_create(EnumEntry);

    token_index++;
    en.name = identifier();

    expect(Tok_OpenCurl);

    while (!tok(Tok_CloseCurl)) {
        EnumEntry entry = {0};
        entry.base.nodebase = node_init();
        entry.base.statementType = Statement_EnumEntry;
        entry.name = identifier();
        if (tok(Tok_Assign)) entry.expr = expectExpression();
        semicolon();

        list_add(en.entries, entry);
    }

    return en;
}

static Struct expectStruct() {
    Struct stru;
    stru.base.nodebase = node_init();
    stru.base.statementType = Statement_Struct;
    stru.fields = list_create(Declaration);

    token_index++;
    stru.name = identifier();
    expect(Tok_OpenCurl);

    do {
        Declaration field;
        field.base.nodebase = node_init();
        field.base.statementType = Statement_Declaration;

        field.include_context = tok(Tok_Keyword_With);
        field.type = expectType();
        field.name = identifier();
        list_add(stru.fields, field);

        while (tok(Tok_Comma)) {
            field.base.nodebase.lineNumber = tokens[token_index].line;
            field.name = identifier();
            list_add(stru.fields, field);
        }

        semicolon();
    } while (tokens[token_index].type != Tok_CloseCurl);

    token_index++;

    return stru;
}

static Typedef expectTypedef() {
    Typedef def = {0};
    def.base.nodebase = node_init();
    def.base.statementType = Statement_Typedef;

    token_index++;
    def.name = identifier();
    if (tok(Tok_Assign)) {
        def.type = expectType();
    }

    semicolon();
    return def;
}

static Declaration expectConst() {
    Declaration decl = {0};
    decl.base.nodebase = node_init();
    decl.base.statementType = Statement_Constant;

    token_index++;
    decl.name = identifier();
    expect(Tok_Assign);
    decl.expr = expectExpression();
    semicolon();
    return decl;
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


static void proc_or_global() {

    Node node = node_init();
    Type* type = expectType();
    Identifier name = identifier();

    if (tok(Tok_OpenParen)) {

        Procedure proc = {0};
        proc.base.nodebase = node;
        proc.base.statementType = Statement_Procedure;
        proc.returnType = type;
        proc.name = name;
        proc.arguments = expectProcArguments();
        proc.scope = expectScope();

        list_add(get_current_file()->namespace->procedures, proc);
        return;
    }


    Declaration decl = {0};
    decl.base.nodebase = node;
    decl.base.statementType = Statement_GlobalVariable;
    decl.type = type;
    decl.name = name;

    globvar:

    if (tok(Tok_OpenSquare)) {
        decl.base.statementType = Statement_FixedArray_Declaration;
        decl.expr = expectExpression();
        expect(Tok_CloseSquare);
    } else {
        if (tok(Tok_Assign)) decl.expr = expectExpression();
    }

    list_add(get_current_file()->namespace->declarations, decl);

    if (tok(Tok_Comma)) {
        decl.base.statementType = Statement_Declaration;
        decl.name = identifier();
        decl.expr = null;
        goto globvar;
    }

    semicolon();
}

static bool parseProgramEntity() {

    switch (tokens[token_index].type) {

        case Tok_Keyword_Include: {
            token_index++;
            expect(Tok_String);
            Identifier string = tokens[token_index - 1].string;
            printf("include \"%s\"\n", get_string(string));
            addFile(get_string(string), null);
            semicolon();
        } break;

        case Tok_Keyword_Struct: {
            Struct stru = expectStruct();
            list_add(get_current_file()->namespace->structs, stru);
        } break;

        case Tok_Keyword_Enum: {
            Enum en = expectEnum();
            list_add(get_current_file()->namespace->enums, en);
        } break;

        case Tok_Keyword_Type: {
            Typedef def = expectTypedef();
            if (def.type) {
                list_add(get_current_file()->namespace->type_defs, def);
            } else {
                list_add(get_current_file()->namespace->opaque_types, def.name);
            }
        } break;

        case Tok_Keyword_Let:
        case Tok_Word: proc_or_global(); break;

        case Tok_Keyword_Const: {
            Declaration decl = expectConst();
            list_add(get_current_file()->namespace->declarations, decl);
        } break;

        case Tok_Keyword_Declare: {
            // function declaration

            token_index++;

            Procedure proc = {0};
            proc.base.nodebase = node_init();
            proc.base.statementType = Statement_Procedure;
            proc.overload = 0;
            proc.scope = null;
            proc.returnType = expectType();
            proc.name = identifier();
            expect(Tok_OpenParen);
            proc.arguments = expectProcArguments();
            semicolon();

            list_add(get_current_file()->namespace->procedures, proc);
        } break;

        case Tok_Keyword_Namespace: {
            error_token("Namespace must be the first thing declared in a file.");
            token_index++;
            tok(Tok_Word);
            tok(Tok_Semicolon);
        } break;

        case Tok_EOF: token_index++; return false;

        default: {
            unexpectedToken();
        } break;
    }

    return true;
}

static void parseFile() {
    token_index = 0;

    if (tok(Tok_Keyword_Namespace)) {
        Identifier name = identifier();
        Namespace* ns = ensureNamespace(name);
        get_current_file()->namespace = ns;
        semicolon();
    } else get_current_file()->namespace = g_Codebase.namespaces[0];

    while (parseProgramEntity());
}


static void parse() {

    codebase_init(&g_Codebase);
    initTypenames();

    parser.unresolved_types = list_create(Type*);
    parser.local_types = list_create(Statement*);


    tokens = list_create(Token);

    for (u32 i = 0; i < list_length(parser.src_files); i++) {
        File* file = &parser.src_files[i];
        parser.current_file_index = i;

        u32 file_size = 0;
        char* content = fileread(file->filename, &file_size);
        if (!content) continue;

        lex(content);
        free(content);

        parseFile();
        list_clear(tokens);
    }

    list_delete(tokens);


    typenode2datatype_globalcheck = true;
    foreach (tp, parser.unresolved_types) {
        Type* type = *tp;
        type->solvedstate = typenode2datatype(type);
        if (type->solvedstate.kind == Typekind_Invalid) {
            error_node(type, "Failed to resolve type");
            printf("    ");
            printType(type);
            printf("\n");
        }
    }
    list_delete(parser.unresolved_types);
    parser.unresolved_types = null;



    { // overloads
        foreach (nsp, g_Codebase.namespaces) {
            Namespace* ns = *nsp;

            u32 procs_length = list_length(ns->procedures);
            for (u32 i = 0; i < procs_length; i++) {
                Procedure* p1 = &ns->procedures[i];

                createSignatureFromProcedure(p1);

                if (p1->overload) continue;

                u32 overload_count = 1;
                Procedure* it = p1;

                // for each subsequent procedure
                for (u32 j = i + 1; j < procs_length; j++) {
                    Procedure* p2 = &ns->procedures[j];
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


}
