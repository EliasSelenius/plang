

static Scope* expectScope();
static Expression* parseExpression();
static Expression* expectExpression();
static Statement* expectStatement();

// asserts the existence of a semicolon
static inline void semicolon() {
    if (tokens[token_index].type != Tok_Semicolon) {
        error_token("Expected semicolon.");
        return;
    }
    token_index++;
}

static Identifier identifier() {
    Token* token = &tokens[token_index];
    if (token->type != Tok_Word) {
        error_token("Invalid identifier.");
        return -1;
    }
    token_index++;
    return token->string;
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


static inline bool funcPtrEquivalence(ProcPtr* a, ProcPtr* b) {
    if (a->arg_count != b->arg_count) return false;
    if (!typeEquals(a->return_type, b->return_type)) return false;
    for (u32 i = 0; i < a->arg_count; i++) {
        if (!typeEquals(a->arg_types[i], b->arg_types[i])) return false;
    }

    return true;
}


/*
static StringBuilder sb_funcPtrName = {0};

static void funcPtrName_append(Datatype type) {
    sbAppendChar(&sb_funcPtrName, '_');
    PlangType* pt = getType(type);
    char* name = getIdentifierStringValue(pt->name);
    sbAppend(&sb_funcPtrName, name);
    for (u32 i = 0; i < type.numPointers; i++) sbAppendChar(&sb_funcPtrName, 'p');
}

static Identifier constructNameForFuncPtr(FuncPtr* funcPtr) {
    sbClear(&sb_funcPtrName);

    sbAppendChar(&sb_funcPtrName, 'f');
    funcPtrName_append(funcPtr->returnType);
    for (u32 i = 0; i < funcPtr->argCount; i++) {
        funcPtrName_append(funcPtr->argTypes[i]);
    }

    StrSpan span;
    span.length = sb_funcPtrName.length;
    span.start = sb_funcPtrName.content;

    Identifier id = appendStringToStringtable(span);
    return id;
}

*/

/*

static Datatype parseProcPtrArgs(Datatype retType) {
    if (!tok(Tok_OpenParen)) return retType;

    // construct a new function pointer by reserving memory for it
    u32 oldLength = g_Codebase.procedure_types->length; // remember the old length in case we have a duplicate
    u32 fpRef = dyReserve(&g_Codebase.procedure_types, sizeof(ProcPtr));

    // arguments
    u32 argCount = 0;
    Datatype argType;
    if (parseType(&argType)) {
        argCount++;
        u32 argRef = dyReserve(&g_Codebase.procedure_types, sizeof(Datatype));
        *(Datatype*)(&g_Codebase.procedure_types->bytes[argRef]) = argType;

        while (tok(Tok_Comma)) {
            argCount++;
            argType = expectType();
            argRef = dyReserve(&g_Codebase.procedure_types, sizeof(Datatype));
            *(Datatype*)(&g_Codebase.procedure_types->bytes[argRef]) = argType;
        }
    }

    ProcPtr* funcPtr = getProcPtr(fpRef);
    funcPtr->returnType = retType;
    funcPtr->argCount = argCount;

    expect(Tok_CloseParen);

    Datatype funcType;
    funcType.kind = Typekind_ProcPtr;
    funcType.ref = fpRef;
    funcType.numPointers = 1;

    // look if the function pointer is a duplicate
    u32 i = 0;
    while (i < oldLength) {
        ProcPtr* f = getProcPtr(i);

        if (funcPtrEquivalence(f, funcPtr)) {
            // is duplicate
            g_Codebase.procedure_types->length = oldLength; // reset
            funcType.ref = i;
            return funcType;
        }

        i += sizeof(ProcPtr) + sizeof(Datatype) * f->argCount;
    }

    return funcType;
}

*/

static Datatype (*expectType)();

static Arena* unresolved_types_arena = &(Arena) {0};

/*
static Datatype expectUnresolvedType() {
    Identifier name = identifier();

    Datatype res = {0};

    UnresolvedType* unresolved = arena_alloc(unresolved_types_arena, sizeof(UnresolvedType));
    unresolved->context = context;
    unresolved->arg_count = 0;
    unresolved->namespace_name = 0;
    unresolved->name = name;
    res = (Datatype) { .kind = Typekind_Unresolved, .unresolved = unresolved };

    if (tok(Tok_Period)) {
        unresolved->namespace_name = name;
        unresolved->name = identifier();
    }

    while (tok(Tok_Mul)) res.numPointers++;

    if (tok(Tok_OpenParen)) {
        if (!tok(Tok_CloseParen)) {
            do {
                Datatype arg = expectUnresolvedType();
                unresolved->arg_count++;
            } while (tok(Tok_Comma));
            expect(Tok_CloseParen);
        }
    }

    return res;
}

*/

static Datatype getType(Namespace* ns, Identifier name) {
    foreach (alias, ns->aliases) {
        if (alias->name == name) return (Datatype) { .kind = Typekind_Alias, .data_ptr = alias, 0 };
    }

    foreach (stru, ns->structs) {
        if (stru->name == name) return (Datatype) { .kind = Typekind_Struct, .data_ptr = stru, 0 };
    }

    foreach (opaque, ns->opaque_types) {
        if (*opaque == name) 
            return (Datatype) { .kind = Typekind_Opaque, .opaque_name = name, 0 };
    }

    return type_invalid;
}
/*
static UnresolvedType* resolveType(UnresolvedType* unres) {

    if (unres->namespace_name == 0) {
        unres->solution = getType(unres->context->namespace, unres->name);
        if (unres->solution.kind == Typekind_Invalid) unres->solution = getType(g_Codebase.namespaces[0], unres->name);
    } else {
        Namespace* ns = getNamespace(unres->namespace_name);
        if (!ns) {
            printf("%s is not a namespace\n", get_string(unres->namespace_name)); // TODO
        }
        unres->solution = getType(ns, unres->name);
    }

    if (unres->solution.kind == Typekind_Invalid) {
        // TODO: unresolved type error
    }

    if (unres->arg_count) {
        u32 arg_count = unres->arg_count;

        UnresolvedType* arg = unres + 1;
        u32 i = 0;
        while (i < arg_count) {
            arg = resolveType(arg);
        }

        for (u32 i = 0; i < arg_count; i++) resolveType(unres + i);

        u32 procId = 0;
        while (procId < g_Codebase.arena_procedure_types.allocated) {
            ProcPtr* ptr = getProcPtr(procId);
            if (ptr->arg_count == arg_count && typeEquals(*datatype, ptr->return_type)) {
                for (u32 i = 0; i < arg_count; i++)
                    if (!typeEquals(unres->arguments[i], ptr->arg_types[i])) goto skip;

                *datatype = (Datatype) { .kind = Typekind_ProcPtr, .procptr = ptr, .numPointers = 1 };
                goto done;
            }
            skip: procId += sizeof(ProcPtr) + sizeof(Datatype) * ptr->arg_count;
        }

        ProcPtr* ptr = arena_alloc(&g_Codebase.arena_procedure_types, sizeof(ProcPtr) + sizeof(Datatype) * arg_count);
        ptr->arg_count = arg_count;
        ptr->return_type = *datatype;
        for (u32 i = 0; i < arg_count; i++)
            ptr->arg_types[i] = unres->arguments[i];
        *datatype = (Datatype) { .kind = Typekind_ProcPtr, .procptr = ptr, .numPointers = 1 };

        done:
        list_delete(unres->arguments);
    }

    free(unres);
}
*/


static Type* newTypeNode(TypeNode node_type) {
    Type* res = malloc(sizeof(Type));
    *res = (Type) { .node_type = node_type, {0} };
    return res;
}

static Type* type_modifier(Type* type);

static Type* test_parsetype() {
    Type* type = newTypeNode(TypeNode_Normal);
    type->context = context;
    type->name = identifier();
    if (tok(Tok_Period)) {
        type->namespace_name = type->name;
        type->name = identifier();
    }

    Type* mod = type;
    while ( (mod = type_modifier(mod)) ) type = mod;

    return type;
}

static Datatype test_expectType() {
    Type* unres = test_parsetype();
    return (Datatype) { .kind = Typekind_Unresolved, .unresolved = unres, 0 };
}

static Type* type_modifier(Type* type) {
    if (tok(Tok_OpenParen)) {
        Type* mod = newTypeNode(TypeNode_Procedure);
        mod->procedure.return_type = type;
        if (tok(Tok_CloseParen)) return mod;

        Type* arg = test_parsetype();
        mod->procedure.arguments = arg;
        while (tok(Tok_Comma)) {
            arg->next = test_parsetype();
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

static Datatype test_type2datatype(Type* type) {
    switch (type->node_type) {
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
            Datatype res = test_type2datatype(type->pointedto);
            res.numPointers++;
            return res;
        } break;
        case TypeNode_Procedure: {

            ProcSignature* sig = createSignature(test_type2datatype(type->procedure.return_type));
            Type* arg = type->procedure.arguments;
            while (arg) {
                addArgument(sig, test_type2datatype(arg), 0);
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
    return type_invalid;
}

static void resolveType(Datatype* datatype) {
    Type* type = datatype->unresolved;
    // printf("Resolved type: ");
    // printType(type);
    // printf("\n");
    *datatype = test_type2datatype(type);
}

static Datatype expectResolvedType() {

    if (tok(Tok_Keyword_Let)) return (Datatype) { .kind = Typekind_MustBeInfered, {0} };

    Datatype res = {0};

    Identifier name = identifier();
    Namespace* ns = getNamespace(name);
    if (ns) {
        expect(Tok_Period);
        name = identifier();
        res = getType(ns, name);
    } else {

             if (name == type_name_int8) res.kind = Typekind_int8;
        else if (name == type_name_uint8) res.kind = Typekind_uint8;
        else if (name == type_name_int16) res.kind = Typekind_int16;
        else if (name == type_name_uint16) res.kind = Typekind_uint16;
        else if (name == type_name_int32) res.kind = Typekind_int32;
        else if (name == type_name_uint32) res.kind = Typekind_uint32;
        else if (name == type_name_int64) res.kind = Typekind_int64;
        else if (name == type_name_uint64) res.kind = Typekind_uint64;
        else if (name == type_name_float32) res.kind = Typekind_float32;
        else if (name == type_name_float64) res.kind = Typekind_float64;
        else if (name == type_name_char) res.kind = Typekind_char;
        else if (name == type_name_void) res.kind = Typekind_void;
        else {
            res = getType(context->namespace, name);
            if (res.kind == Typekind_Invalid) res = getType(g_Codebase.namespaces[0], name);
        }
    }

    if (res.kind == Typekind_Invalid) {
        error_temp("Could not find type \"%s\"", get_string(name));
    }

    while (tok(Tok_Mul)) res.numPointers++;

    if (tok(Tok_OpenParen)) {
        // ProcSignature* sig = createSignature(res);
        if (!tok(Tok_CloseParen)) {
            do {
                Datatype arg = expectResolvedType();
            } while (tok(Tok_Comma));
            expect(Tok_CloseParen);
        }
    }

    return res;
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


// tok_scan("t t t", Tok_Let, Tok_Word, Tok_Assign)
static bool tok_scan(char* format, ...) {
    u32 start = token_index;

    va_list args;
    va_start(args, format);

    TokenType type;
    char mode = ' ';

    u32 formatIndex = 0;
    while (true) {
        char cmd = format[formatIndex++];
        if (cmd == '\0') return true;
        if (cmd == ' ') continue;

        if (cmd == 't') {
            type = va_arg(args, TokenType);

            if (mode == ' ') {
                if (tokens[token_index].type == type) {
                    token_index++;
                    continue;
                }

                goto fail;
            } else if (mode == '*') {
                while (tokens[token_index].type == type) token_index++;
                mode = ' ';
                continue;
            }

        }

        if (cmd == '*') {
            mode = '*'; continue;
        }

        printf("Wrong format in tok_scan()\n");
    }

    fail:
    token_index = start;
    return false;
}

static void expectFuncCallArgs(ProcCall* func, Expression* proc_expr) {
    func->proc_expr = proc_expr;

    Expression* expr = parseExpression();
    if (expr) {
        func->args = list_create(Expression*);
        list_add(func->args, expr);

        while (tok(Tok_Comma)) {
            expr = expectExpression();
            list_add(func->args, expr);
        }
    } else {
        func->args = null;
    }

    expect(Tok_CloseParen);
}


static Expression* parseLeafExpression() {
    Expression* res = null;

    UnaryExpression* unary = null;
    switch (tokens[token_index].type) {
        case Tok_Mul: unary = allocExpr(ExprType_Unary_AddressOf); token_index++; break;
        case Tok_At: unary = allocExpr(ExprType_Unary_ValueOf); token_index++; break;
        case Tok_ExclamationMark: unary = allocExpr(ExprType_Unary_Not); token_index++; break;
        case Tok_PlusPlus: unary = allocExpr(ExprType_Unary_PreIncrement); token_index++; break;
        case Tok_MinusMinus: unary = allocExpr(ExprType_Unary_PreDecrement); token_index++; break;
        case Tok_Minus: unary = allocExpr(ExprType_Unary_Negate); token_index++; break;
        case Tok_Tilde: unary = allocExpr(ExprType_Unary_BitwiseNot); token_index++; break;
        default: break;
    }

    switch (tokens[token_index].type) {
        case Tok_Word: {
            VariableExpression* ve = allocExpr(ExprType_Variable);

            Identifier name = identifier();
            Namespace* ns = getNamespace(name);
            if (ns) {
                expect(Tok_Period);
                name = identifier();
                ve->ref = getReference(ns, name);
            } else {
                ve->ref = getReference(context->namespace, name);
                if (ve->ref.reftype == RefType_Invalid) {
                    ve->ref = getReference(g_Codebase.namespaces[0], name);
                }

                if (ve->ref.reftype == RefType_Invalid) {

                    

                    ve->ref.name = name;
                }
            }

            res = (Expression*)ve;
        } break;

        case Tok_Keyword_Alloc: {
            AllocExpression* alloc = allocExpr(ExprType_Alloc);
            token_index++;
            alloc->type = expectType();
            alloc->sizeExpr = null;
            if (tok(Tok_OpenSquare)) {
                alloc->sizeExpr = expectExpression();
                expect(Tok_CloseSquare);
            }

            res = (Expression*)alloc;
        } break;

        case Tok_Keyword_Sizeof: {
            SizeofExpression* sof = allocExpr(ExprType_Sizeof);
            token_index++;

            bool mustClose = false;
            if (tok(Tok_OpenParen)) mustClose = true;
            sof->type = expectType();
            if (mustClose) expect(Tok_CloseParen);

            res = (Expression*)sof;
        } break;

        case Tok_OpenParen: {
            ParenthesizedExpression* p = allocExpr(ExprType_Parenthesized);
            token_index++;
            p->innerExpr = expectExpression();
            expect(Tok_CloseParen);

            res = (Expression*)p;
        } break;


        case Tok_Integer: {
            LiteralExpression* lit = allocExpr(ExprType_Literal_Integer);
            lit->integer = tokens[token_index++].integer;
            res = (Expression*)lit;
        } break;
        case Tok_Decimal: {
            LiteralExpression* lit = allocExpr(ExprType_Literal_Decimal);
            lit->decimal = tokens[token_index++].decimal;
            res = (Expression*)lit;
        } break;
        case Tok_String: {
            LiteralExpression* lit = allocExpr(ExprType_Literal_String);
            lit->string = tokens[token_index++].string;
            res = (Expression*)lit;
        } break;
        case Tok_Char: {
            LiteralExpression* lit = allocExpr(ExprType_Literal_Char);
            lit->character = tokens[token_index++].character;
            res = (Expression*)lit;
        } break;

        case Tok_Keyword_True:  res = allocExpr(ExprType_Literal_True); token_index++; break;
        case Tok_Keyword_False: res = allocExpr(ExprType_Literal_False); token_index++; break;
        case Tok_Keyword_Null:  res = allocExpr(ExprType_Literal_Null); token_index++; break;

        default: return null;
    }

    while (true) {
        switch (tokens[token_index].type) {
            case Tok_Period: {
                DerefOperator* deref = allocExpr(ExprType_Deref);
                token_index++;

                deref->expr = res;
                deref->name = identifier();
                res = (Expression*)deref;
            } continue;

            case Tok_OpenSquare: {
                IndexingExpression* ind = allocExpr(ExprType_Indexing);
                token_index++;

                ind->indexed = res;
                ind->index = expectExpression();
                expect(Tok_CloseSquare);
                res = (Expression*)ind;
            } continue;

            case Tok_OpenParen: {
                ProcCall* call = allocExpr(ExprType_ProcCall);
                token_index++;

                expectFuncCallArgs(call, res);
                res = (Expression*)call;
            } continue;

            default: break;
        }
        break;
    }

    if (unary) {
        unary->expr = res;
        res = (Expression*)unary;
    }

    switch (tokens[token_index].type) {
        case Tok_PlusPlus: {
            UnaryExpression* postInc = allocExpr(ExprType_Unary_PostIncrement);
            token_index++;
            postInc->expr = res;
            res = (Expression*)postInc;
        } break;

        case Tok_MinusMinus: {
            UnaryExpression* postDec = allocExpr(ExprType_Unary_PostDecrement);
            token_index++;
            postDec->expr = res;
            res = (Expression*)postDec;
        } break;

        default: break;
    }

    if (tokens[token_index].type == Tok_Keyword_As) {
        CastExpression* cast = allocExpr(ExprType_Cast);
        token_index++;
        cast->expr = res;
        cast->castToType = expectType();
        res = (Expression*)cast;
    }

    return res;
}

static Expression* testForTernary(Expression* expr) {
    if (tokens[token_index].type != Tok_QuestionMark) return expr;

    TernaryExpression* ter = allocExpr(ExprType_Ternary);
    token_index++;

    ter->condition = expr;
    ter->thenExpr = expectExpression();
    expect(Tok_Colon);
    ter->elseExpr = expectExpression();

    return (Expression*)ter;
}

static Expression* expectExpression() {
    Expression* res = parseExpression();
    if (res == null) {
        error_token("Expected expression.");
        token_index++; // TODO: why am I incrementing the token index here, but not in expectLeafExpression?
    }
    return res;
}

static u32 operatorPriority(ExprType type) {
    switch (type) {

        case ExprType_Bitwise_Lshift:
        case ExprType_Bitwise_Rshift:
        case ExprType_Bitwise_And:
        case ExprType_Bitwise_Or:
        case ExprType_Bitwise_Xor:
            return 1;

        case ExprType_BooleanAnd:
        case ExprType_BooleanOr:
            return 2;

        case ExprType_Less:
        case ExprType_Greater:
        case ExprType_LessEquals:
        case ExprType_GreaterEquals:
        case ExprType_Equals:
        case ExprType_NotEquals:
            return 3;

        case ExprType_Plus:
        case ExprType_Minus:
            return 4;
        case ExprType_Mul:
        case ExprType_Div:
        case ExprType_Mod:
            return 5;

        default: return 0; // not an operator
    }
}

static inline u32 binaryOperatorPriority(BinaryExpression* expr) {
    return operatorPriority(expr->base.expressionType);
}
static inline bool isBinaryExpression(Expression* expr) {
    return operatorPriority(expr->expressionType) != 0;
}

static Expression* expectLeafExpression() {
    Expression* res = parseLeafExpression();
    if (!res) error_token("Expected expression.");
    return res;
}

static ExprType getExprTypeForBinaryOperator(TokenType type) {
    switch (type) {
        case Tok_Plus: return ExprType_Plus;
        case Tok_Minus: return ExprType_Minus;
        case Tok_Mul: return ExprType_Mul;
        case Tok_Div: return ExprType_Div;
        case Tok_Mod: return ExprType_Mod;

        case Tok_LessThan: return ExprType_Less;
        case Tok_GreaterThan: return ExprType_Greater;
        case Tok_LessThanOrEqual: return ExprType_LessEquals;
        case Tok_GreaterThanOrEqual: return ExprType_GreaterEquals;
        case Tok_Equals: return ExprType_Equals;
        case Tok_NotEquals: return ExprType_NotEquals;
        case Tok_Keyword_And: return ExprType_BooleanAnd;
        case Tok_Keyword_Or: return ExprType_BooleanOr;

        case Tok_Ampersand: return ExprType_Bitwise_And;
        case Tok_Pipe: return ExprType_Bitwise_Or;
        case Tok_Caret: return ExprType_Bitwise_Xor;
        case Tok_LeftShift: return ExprType_Bitwise_Lshift;
        case Tok_RightShift: return ExprType_Bitwise_Rshift;

        default: return 0; // not an operator
    }
}


static BinaryExpression* appendBinaryExpression(BinaryExpression* target, BinaryExpression* addition) {
    u32 targetPriority = binaryOperatorPriority(target);
    u32 additionPriority = binaryOperatorPriority(addition);

    if (additionPriority > targetPriority) {
        if (isBinaryExpression(target->right)) {
            target->right = (Expression*)appendBinaryExpression((BinaryExpression*)target->right, addition);
            return target;
        } else {
            addition->left = target->right;
            target->right = (Expression*)addition;
            return target;
        }
    } else {
        addition->left = (Expression*)target;
        return addition;
    }

    return null; // should never happen
}

static Expression* parseExpression() {
    Expression* a = parseLeafExpression();
    if (!a) return null;

    TokenType tokentype = tokens[token_index].type;
    ExprType exprType = getExprTypeForBinaryOperator(tokentype);
    if (!exprType) return testForTernary(a);

    BinaryExpression* root = allocExpr(exprType);
    token_index++;
    root->left = a;
    root->right = expectLeafExpression();

    tokentype = tokens[token_index].type;
    while ( (exprType = getExprTypeForBinaryOperator(tokentype)) ) {

        BinaryExpression* op = allocExpr(exprType);
        token_index++;
        op->right = expectLeafExpression();

        root = appendBinaryExpression(root, op);

        tokentype = tokens[token_index].type;
    }

    return testForTernary((Expression*)root);
}

static IfStatement* expectIfStatement() {
    IfStatement* res = malloc(sizeof(IfStatement));
    res->base.nodebase.filepath = g_Filename;
    res->base.statementType = Statement_If;
    res->next = null;
    res->condition = expectExpression();

    res->statement = expectStatement();

    if (tok(Tok_Keyword_Else)) {
        if (tok(Tok_Keyword_If)) {
            // TODO: line number
            res->next = expectIfStatement();
        } else {
            IfStatement* elseStatement = malloc(sizeof(IfStatement));
            elseStatement->base.statementType = Statement_If;
            elseStatement->condition = null;
            elseStatement->next = null;
            elseStatement->statement = expectStatement();

            res->next = elseStatement;
        }
    }

    return res;
}

static bool isBasicType() {
    if (tok(Tok_Word)) {
        while (tok(Tok_Mul));

        // funcptr
        if (tok(Tok_OpenParen)) {
            if (tok(Tok_CloseParen)) return true;

            do {
                if (!isBasicType()) return false;
            } while (tok(Tok_Comma));

            if (tok(Tok_CloseParen)) return true;
            else return false;
        }

        return true;
    }

    return false;
}

static u32 peekType() {
    u32 ti = token_index;

    if (isBasicType()) {
        u32 res = token_index;
        token_index = ti;
        return res;
    } else {
        token_index = ti;
        return 0;
    }
}

static VarDecl* expectVarDecl() {
    VarDecl* decl = malloc(sizeof(VarDecl));
    decl->base.statementType = Statement_Declaration;
    decl->type = expectType();
    decl->name = identifier();

    decl->assignmentOrNull = null;
    if (tok(Tok_Assign)) {
        decl->assignmentOrNull = expectExpression();
    } else if (decl->type.kind == Typekind_MustBeInfered) {
        error_token("Variable \"%s\" must be assigned to, to be type inferred.", get_string(decl->name));
    }

    return decl;
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

static Statement* expectStatement() {
    switch (tokens[token_index].type) {

        case Tok_OpenCurl: return (Statement*)expectScope();

        case Tok_Keyword_While: {
            WhileStatement* whileStatement = allocStatement(Statement_While);
            token_index++;

            whileStatement->condition = expectExpression();
            whileStatement->statement = expectStatement();

            return (Statement*)whileStatement;
        }

        case Tok_Keyword_If: {
            token_index++;
            return (Statement*)expectIfStatement();
        }

        case Tok_Keyword_For: {
            ForInStatement* forin = allocStatement(Statement_ForIn);
            token_index++;

            bool mustClose = tok(Tok_OpenParen);

            if (tokens[token_index + 1].type == Tok_Keyword_In) {
                forin->index_type = type_int32;
                forin->index_name = identifier();
                token_index++;
            } else {
                forin->index_type = expectType();
                forin->index_name = identifier();
                expect(Tok_Keyword_In);
            }

            forin->min_expr = expectExpression();
            expect(Tok_Dotdot);
            forin->max_expr = expectExpression();

            if (mustClose) expect(Tok_CloseParen);

            forin->statement = expectStatement();
            return (Statement*)forin;
        }

        case Tok_Keyword_Switch: {
            SwitchStatement* switchStatement = allocStatement(Statement_Switch);
            token_index++;

            switchStatement->expr = expectExpression();
            switchStatement->scope = expectScope();

            return (Statement*)switchStatement;
        }

        case Tok_Keyword_Case: {
            CaseLabelStatement* caseLabel = allocStatement(Statement_CaseLabel);
            token_index++;
            caseLabel->expr = expectExpression();
            expect(Tok_Colon);
            return (Statement*)caseLabel;
        }

        case Tok_Keyword_Default: {
            Statement* sta = allocStatement(Statement_DefaultLabel);
            token_index++;
            expect(Tok_Colon);
            return sta;
        }

        case Tok_Keyword_Continue: {
            Statement* sta = allocStatement(Statement_Continue);
            token_index++;
            semicolon();
            return sta;
        }

        case Tok_Keyword_Break: {
            Statement* sta = allocStatement(Statement_Break);
            token_index++;
            semicolon();
            return sta;
        }

        case Tok_Keyword_Return: {
            ReturnStatement* ret = allocStatement(Statement_Return);
            token_index++;
            ret->returnExpr = parseExpression();
            semicolon();
            return (Statement*)ret;
        }

        case Tok_Keyword_Goto: {
            GotoStatement* go = allocStatement(Statement_Goto);
            token_index++;
            go->label = identifier();
            semicolon();
            return (Statement*)go;
        }

        case Tok_Keyword_Let: {
            VarDecl* decl = expectVarDecl();
            semicolon();
            return (Statement*)decl;
        }

        default: break;
    }

    // label
    if (tokens[token_index].type == Tok_Word && tokens[token_index + 1].type == Tok_Colon) {
        LabelStatement* label = allocStatement(Statement_Label);
        label->label = tokens[token_index++].string;
        token_index++;
        return (Statement*)label;
    }

    // declaration
    u32 postType = peekType();
    if (postType && tokens[postType].type == Tok_Word) {

        if (tokens[postType + 1].type == Tok_OpenParen) {
            // confirmed procedure

            LocalProc* localproc = allocStatement(Statement_LocalProc);
            localproc->proc.overload = 0;
            localproc->proc.returnType = expectType();
            localproc->proc.name = identifier();
            expect(Tok_OpenParen);
            localproc->proc.arguments = expectProcArguments();
            localproc->proc.scope = expectScope();

            return (Statement*)localproc;
        } else {
            // confirmed declaration

            VarDecl* decl = allocStatement(Statement_Declaration);
            decl->type = expectType();
            decl->name = identifier();

            // fixed sized arrays
            if (tok(Tok_OpenSquare)) {
                decl->base.statementType = Statement_FixedArray_Declaration;
                Expression* sizeExpr = expectExpression();
                expect(Tok_CloseSquare);
                decl->assignmentOrNull = sizeExpr;
                decl->type.numPointers++;
            } else {
                decl->assignmentOrNull = tok(Tok_Assign) ? expectExpression() : null;
            }

            semicolon();
            return (Statement*)decl;
        }
    }


    // expression statement
    Expression* expr = parseExpression();
    if (!expr) {
        unexpectedToken();
        return null;
    }

    switch (expr->expressionType) {
        case ExprType_Unary_PreIncrement:
        case ExprType_Unary_PostIncrement:
        case ExprType_Unary_PreDecrement:
        case ExprType_Unary_PostDecrement:
        case ExprType_ProcCall: {
            StatementExpression* staExpr = allocStatement(Statement_Expression);
            staExpr->base.nodebase.lineNumber = expr->nodebase.lineNumber;
            staExpr->expr = expr;
            semicolon();
            return (Statement*)staExpr;
        }

        case ExprType_Indexing:
        case ExprType_Unary_ValueOf:
        case ExprType_Variable:
        case ExprType_Deref: {

            Token* token = anyof(8, Tok_Assign,
                                    Tok_PlusAssign,
                                    Tok_MinusAssign,
                                    Tok_MulAssign,
                                    Tok_DivAssign,
                                    Tok_BitAndAssign,
                                    Tok_BitOrAssign,
                                    Tok_BitXorAssign);
            if (token) {
                Assignement* ass = allocStatement(Statement_Assignment);
                ass->base.nodebase.lineNumber = expr->nodebase.lineNumber;
                ass->assigneeExpr = expr;
                ass->assignmentOper = token->type;
                ass->expr = expectExpression();
                semicolon();
                return (Statement*)ass;
            }

            error_token("Expected an assignment.");
        } break;


        default:
            error_token("This expression is all by its lonesome.");
            return null;
    }

    return null;
}

static Scope* activateScope(Scope* scope) {
    static Scope* current = null;

    Scope* old = current;
    current = scope;
    return old;
}

static Scope* expectScope() {

    Scope* scope = allocStatement(Statement_Scope);
    scope->statements = list_create(Statement*);
    scope->parentScope = activateScope(scope);
    expect(Tok_OpenCurl);

    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            list_add(scope->statements, statement);
        } else {
            // there must have been an error.
        }
    }

    activateScope(scope->parentScope);
    return scope;
}

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

typedef enum ParseJobKind {
    ParseJobKind_Scope,
    ParseJobKind_Expression
} ParseJobKind;

typedef struct ParseJob {
    File* context;
    ParseJobKind kind;
    u32 token_index;
} ParseJob;

static ParseJob* createParseJob(ParseJobKind kind) {
    ParseJob* job = malloc(sizeof(ParseJob));
    job->context = context;
    job->token_index = token_index;
    job->kind = kind;
    return job;
}

static void* executeParseJob(ParseJob* job) {
    token_index = job->token_index;
    context = job->context;
    ParseJobKind kind = job->kind;
    free(job);

    switch (kind) {
        case ParseJobKind_Scope:
            activateScope(null);
            return expectScope();
        case ParseJobKind_Expression:
            return expectExpression();
        default: break;
    }

    return null;
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
    Datatype type = expectType();
    Identifier name = identifier();

    if (tok(Tok_OpenParen)) {
        // function

        Procedure proc = {0};
        proc.overload = 0;
        proc.next_overload = null;
        proc.returnType = type;
        proc.name = name;
        proc.arguments = expectProcArguments();

        proc.pre_scope_data.begin_scope_token = token_index;
        proc.pre_scope_data.context = context;
        expect(Tok_OpenCurl);
        skipBody();

        list_add(context->namespace->procedures, proc);

    } else {
        // global variable
        VarDecl decl = {0};
        decl.base.statementType = Statement_Declaration;
        decl.assignmentOrNull = null;
        decl.name = name;
        decl.type = type;

        globvar:
        if (tok(Tok_Assign)) {
            // decl.assignmentOrNull = expectExpression();
            decl.assignmentOrNull = (Expression*)createParseJob(ParseJobKind_Expression);
            while (tokens[token_index].type != Tok_Semicolon && tokens[token_index].type != Tok_Comma) token_index++;
        } else if (decl.type.kind == Typekind_MustBeInfered) {
            error_token("Global variable \"%s\" must be assigned to, to be type inferred.", get_string(decl.name));
        }

        list_add(context->namespace->global_variables, decl);

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
            u32 struLen = list_length(context->namespace->structs);
            for (u32 i = 0; i < struLen; i++) {
                if (context->namespace->structs[i].name == stru.name) {
                    error_node(&stru, "Struct \"%s\" is already defined.", get_string(stru.name));
                    break;
                }
            }

            list_add(context->namespace->structs, stru);

        } break;

        case Tok_Keyword_Type: {
            u32 lineNum = tokens[token_index].line;
            token_index++;

            AliasType alias = {0};
            alias.name = identifier();
            if (tok(Tok_Assign)) {
                alias.aliasedType = expectType();
                list_add(context->namespace->aliases, alias);
            } else {
                list_add(context->namespace->opaque_types, alias.name);
            }
            semicolon();

        } break;

        case Tok_Keyword_Const: {
            u32 lineNum = tokens[token_index].line;
            token_index++;

            Constant constant = {0};
            constant.name = identifier();
            expect(Tok_Assign);
            constant.expr = (Expression*)createParseJob(ParseJobKind_Expression);
            // constant.expr = expectExpression();
            list_add(context->namespace->constants, constant);

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

            list_add(context->namespace->procedures, proc);
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
    if (tok(Tok_Keyword_Namespace)) {
        Identifier name = identifier();
        Namespace* ns = ensureNamespace(name);
        context->namespace = ns;
        semicolon();
    } else context->namespace = g_Codebase.namespaces[0];

    while (parseProgramEntity());
}


static void parse() {

    // expectType = expectUnresolvedType;
    expectType = test_expectType;

    *unresolved_types_arena = arena_create();

    u32 tokens_length = list_length(tokens);
    u32 file_index = 0;
    token_index = 0;
    while (token_index < tokens_length) {
        context = &Files[file_index++];
        parseFile();
    }


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


    expectType = expectResolvedType;

    iterate(constants, {
        item->expr = executeParseJob((ParseJob*)item->expr);
    });

    iterate(global_variables, {
        if (item->assignmentOrNull == null) continue;
        item->assignmentOrNull = executeParseJob((ParseJob*)item->assignmentOrNull);
    });

    foreach (ns, g_Codebase.namespaces) {
        foreach (proc, (*ns)->procedures) {
            token_index = proc->pre_scope_data.begin_scope_token;
            if (token_index) {
                context = proc->pre_scope_data.context;
                activateScope(null);
                proc->scope = expectScope();
            }
        }
    }
}
