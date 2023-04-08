

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
        error_token("Invalid identifier.");
        return -1;
    }
    token_index++;
    return token.string;
}

static void expect(TokenType type) {
    if (tokens[token_index].type != type) {
        error_token("Unexpected token type %s.", TokenType_Names[tokens[token_index].type]);
        return;
    }
    token_index++;
}

static void unexpectedToken() {
    error_token("Unexpected token.");
    token_index++;
}

static inline bool tok(TokenType type) {
    if (tokens[token_index].type == type) {
        token_index++;
        return true;
    }
    return false;
}


static Datatype getType(Namespace* ns, Identifier name) {
    foreach (alias, ns->aliases) {
        if (alias->name == name) return (Datatype) { .kind = Typekind_Alias, .data_ptr = alias, 0 };
    }

    foreach (stru, ns->structs) {
        if (stru->name == name) return (Datatype) { .kind = Typekind_Struct, .data_ptr = stru, 0 };
    }

    foreach (opaque, ns->opaque_types) {
        if (*opaque == name) {
            Datatype res = { .kind = Typekind_Opaque, .data_ptr = (void*)name, 0 };
            return res;
        }
    }

    return type_invalid;
}


static Type* newTypeNode(TypeNode node_type) {
    Type* res = malloc(sizeof(Type));
    *res = (Type) { .node_type = node_type, {0} };
    return res;
}

static Type* type_modifier(Type* type);

static Type* parseTypeNode() {
    Type* type = newTypeNode(TypeNode_Normal);
    type->context = parser.current_file;

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

static Type* expectType() {
    Type* type = parseTypeNode();
    list_add(parser.unresolved_types, type);
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

static Datatype typenode2datatype(Type* type) {
    switch (type->node_type) {
        case TypeNode_MustInfer: return type_invalid;
        case TypeNode_Normal: {
            if (type->namespace_name == 0) {

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

                Datatype res = getType(type->context->namespace, type->name);
                if (res.kind == Typekind_Invalid) res = getType(g_Codebase.namespaces[0], type->name);

                if (res.kind == Typekind_Invalid) goto fail;

                return res;
            }

            Namespace* ns = getNamespace(type->namespace_name);
            Datatype res = getType(ns, type->name);

            if (res.kind == Typekind_Invalid) goto fail;
            return res;
        } break;
        case TypeNode_Pointer: {
            Datatype res = typenode2datatype(type->pointedto);
            res.numPointers++;
            return res;
        } break;
        case TypeNode_Procedure: {

            ProcSignature* sig = createSignature(typenode2datatype(type->procedure.return_type));
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
        } break;
    }

    fail:
    error_temp("Failed to resolve type");
    printType(type);
    printf("\n");
    return type_invalid;
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


static ProcArg* expectProcArguments() {

    if (tok(Tok_CloseParen)) {
        return null;
    }

    ProcArg* res = list_create(ProcArg);
    ProcArg arg;
    do {
        arg.type = expectType();
        arg.name = identifier();
        list_add(res, arg);
    } while (tok(Tok_Comma));
    expect(Tok_CloseParen);

    return res;
}

#include "parser_expressions.c"
#include "parser_statements.c"


static PlangStruct expectStruct() {
    PlangStruct stru;
    stru.name = identifier();
    stru.fields = list_create(Field);

    expect(Tok_OpenCurl);

    do {
        Field field;
        field.nodebase.lineNumber = tokens[token_index].line;
        field.type = expectType();
        field.name = identifier();
        list_add(stru.fields, field);

        while (tok(Tok_Comma)) {
            field.nodebase.lineNumber = tokens[token_index].line;
            field.name = identifier();
            list_add(stru.fields, field);
        }

        semicolon();
    } while (tokens[token_index].type != Tok_CloseCurl);

    token_index++;

    return stru;
}


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

static void funcOrGlobal() {
    Type* type = expectType();
    Identifier name = identifier();

    if (tok(Tok_OpenParen)) {
        // function

        Procedure proc = {0};
        proc.overload = 0;
        proc.next_overload = null;
        proc.returnType = type;
        proc.name = name;
        proc.arguments = expectProcArguments();
        proc.scope = expectScope();

        list_add(parser.current_file->namespace->procedures, proc);

    } else {
        // global variable
        VarDecl decl = {0};
        decl.base.statementType = Statement_Declaration;
        decl.assignmentOrNull = null;
        decl.name = name;
        decl.type = type;

        globvar:
        if (tok(Tok_Assign)) {
            decl.assignmentOrNull = expectExpression();
        } else if (decl.type->node_type == TypeNode_MustInfer) {
            error_token("Global variable \"%s\" must be assigned to, to be type inferred.", get_string(decl.name));
        }

        list_add(parser.current_file->namespace->global_variables, decl);

        if (tok(Tok_Comma)) {
            decl.name = identifier();
            decl.assignmentOrNull = null;
            goto globvar;
        }

        semicolon();
    }
}

static bool parseProgramEntity() {

    switch (tokens[token_index].type) {

        case Tok_Keyword_Struct: {
            // struct
            u32 lineNum = tokens[token_index].line;
            token_index++;
            PlangStruct stru = expectStruct();
            stru.nodebase.lineNumber = lineNum;
            u32 struLen = list_length(parser.current_file->namespace->structs);
            for (u32 i = 0; i < struLen; i++) {
                if (parser.current_file->namespace->structs[i].name == stru.name) {
                    error_node(&stru, "Struct \"%s\" is already defined.", get_string(stru.name));
                    break;
                }
            }

            list_add(parser.current_file->namespace->structs, stru);

        } break;

        case Tok_Keyword_Type: {
            u32 lineNum = tokens[token_index].line;
            token_index++;

            AliasType alias = {0};
            alias.name = identifier();
            if (tok(Tok_Assign)) {
                alias.aliasedType = expectType();
                list_add(parser.current_file->namespace->aliases, alias);
            } else {
                list_add(parser.current_file->namespace->opaque_types, alias.name);
            }
            semicolon();

        } break;

        case Tok_Keyword_Const: {
            u32 lineNum = tokens[token_index].line;
            token_index++;

            Constant constant = {0};
            constant.name = identifier();
            expect(Tok_Assign);
            constant.expr = expectExpression();
            list_add(parser.current_file->namespace->constants, constant);

            while (tokens[token_index++].type != Tok_Semicolon);
        } break;

        case Tok_Keyword_Let:
        case Tok_Word: funcOrGlobal(); break;

        case Tok_Keyword_Declare: {
            // function declaration

            token_index++;

            Procedure proc = {0};
            proc.overload = 0;
            proc.scope = null;
            proc.returnType = expectType();
            proc.name = identifier();
            expect(Tok_OpenParen);
            proc.arguments = expectProcArguments();
            semicolon();

            list_add(parser.current_file->namespace->procedures, proc);
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
        parser.current_file->namespace = ns;
        semicolon();
    } else parser.current_file->namespace = g_Codebase.namespaces[0];

    while (parseProgramEntity());
}


static void parse(File* files) {

    codebase_init(&g_Codebase);
    initTypenames();

    parser = (Parser){0};
    parser.src_files = files;
    parser.unresolved_types = list_create(Type*);


    tokens = list_create(Token);

    foreach (file, files) {
        u32 file_size = 0;
        char* content = fileread(file->filename, &file_size);
        lex(content);
        free(content);

        parser.current_file = file;
        parseFile();
        list_clear(tokens);
    }

    list_delete(tokens);


    foreach (tp, parser.unresolved_types) {
        Type* type = *tp;
        type->solvedstate = typenode2datatype(type);
    }
    list_delete(parser.unresolved_types);
    parser.unresolved_types = null;

    /*
    { // resolve types
        // TODO: maybe acumulate unresolved types in a list and iterate that, instead of having this much loop nesting

        foreach (nsp, g_Codebase.namespaces) {
            Namespace* ns = *nsp;

            foreach (alias, ns->aliases) {
                resolveType(&alias->aliasedType);
            }

            foreach (proc, ns->procedures) {
                resolveType(&proc->returnType);
                if (proc->arguments) {
                    foreach (arg, proc->arguments) {
                        resolveType(&arg->type);
                    }
                }
            }

            foreach (glob, ns->global_variables) {
                resolveType(&glob->type);
            }

            foreach (stru, ns->structs) {
                foreach (field, stru->fields) {
                    resolveType(&field->type);
                }
            }
        }
    }
    */

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
