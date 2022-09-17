
static void expectBlock(Codeblock* scope);
static Expression* parseExpression();
static Expression* expectExpression();
static bool parseType(Datatype* type);
static Datatype expectType();

static u32 type_name_int8 = 0;
static u32 type_name_uint8 = 0;
static u32 type_name_int16 = 0;
static u32 type_name_uint16 = 0;
static u32 type_name_int32 = 0;
static u32 type_name_uint32 = 0;
static u32 type_name_int64 = 0;
static u32 type_name_uint64 = 0;
static u32 type_name_float32 = 0;
static u32 type_name_float64 = 0;
static u32 type_name_char = 0;
static u32 type_name_void = 0;

static void initTypenames() {
    type_name_int8    = appendStringToStringtable(spFrom("int8"));
    type_name_uint8   = appendStringToStringtable(spFrom("uint8"));
    type_name_int16   = appendStringToStringtable(spFrom("int16"));
    type_name_uint16  = appendStringToStringtable(spFrom("uint16"));
    type_name_int32   = appendStringToStringtable(spFrom("int32"));
    type_name_uint32  = appendStringToStringtable(spFrom("uint32"));
    type_name_int64   = appendStringToStringtable(spFrom("int64"));
    type_name_uint64  = appendStringToStringtable(spFrom("uint64"));
    type_name_float32 = appendStringToStringtable(spFrom("float32"));
    type_name_float64 = appendStringToStringtable(spFrom("float64"));
    type_name_char    = appendStringToStringtable(spFrom("char"));
    type_name_void    = appendStringToStringtable(spFrom("void"));
}

static u32 token_index = 0;


// asserts the existence of a semicolon
static inline void semicolon() {
    if (tokens[token_index].type != Tok_Semicolon) {     
        // error("Expected semicolon, but got \"%.*s\" instead.",
            // tokens[token_index].value.length,
            // tokens[token_index].value.start);

        error("Expected semicolon.");
        return;
    }
    token_index++;
}

static Identifier identifier() {
    Token* token = &tokens[token_index];
    if (token->type != Tok_Word) {
        // error("\"%.*s\" is not a valid identifier.",
            // token->value.length,
            // token->value.start);
        error("Invalid identifier.");
        return -1;
    }
    token_index++;
    return token->stringTableByteOffset;
}

static void expect(TokenType type) {
    if (tokens[token_index].type != type) {
        // TODO: get TokenType as string
        // error("Expected token type %d, but got \"%.*s\" instead.",
            // type,
            // tokens[token_index].value.length,
            // tokens[token_index].value.start);

        error("Unexpected token type %d.", tokens[token_index].type);

        return;
    }
    token_index++;
}

static void unexpectedToken() {

    // error("Unexpected token \"%.*s\".",
        // tokens[token_index].value.length,
        // tokens[token_index].value.start);

    error("Unexpected token.");

    token_index++;
}

static inline bool tok(TokenType type) {
    if (tokens[token_index].type == type) {
        token_index++;
        return true;
    }
    return false;
}


static inline bool funcPtrEquivalence(FuncPtr* a, FuncPtr* b) {
    if (a->argCount != b->argCount) return false;
    if (!typeEquals(a->returnType, b->returnType)) return false;
    for (u32 i = 0; i < a->argCount; i++) {
        if (!typeEquals(a->argTypes[i], b->argTypes[i])) return false;
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


static Datatype ensureFuncPtrExistsFromFuncDeclaration(FuncDeclaration* decl) {

    u32 oldLength = g_Unit->funcPtrTypes->length; // remember the old length in case we have a duplicate
    u32 fpRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(FuncPtr));

    u32 argCount = decl->arguments ? darrayLength(decl->arguments) : 0;
    for (u32 i = 0; i < argCount; i++) {
        u32 argRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(Datatype));
        *(Datatype*)(&g_Unit->funcPtrTypes->bytes[argRef]) = decl->arguments[i].type;
    }

    FuncPtr* funcPtr = getFuncPtr(fpRef);
    funcPtr->returnType = decl->returnType;
    funcPtr->argCount = argCount;

    Datatype funcType;
    funcType.kind = Typekind_FuncPtr;
    funcType.ref = fpRef;
    funcType.numPointers = 1;

    // look if the function pointer is a duplicate
    u32 i = 0;
    while (i < oldLength) {
        FuncPtr* f = getFuncPtr(i);

        if (funcPtrEquivalence(f, funcPtr)) {
            // is duplicate
            g_Unit->funcPtrTypes->length = oldLength; // reset
            funcType.ref = i;
            return funcType;
        }

        i += sizeof(FuncPtr) + sizeof(Datatype) * f->argCount;
    }

    return funcType;
}


static Datatype parseFuncPtrArgs(Datatype retType) {
    if (!tok(Tok_OpenParen)) return retType;

    // construct a new function pointer by reserving memory for it
    u32 oldLength = g_Unit->funcPtrTypes->length; // remember the old length in case we have a duplicate
    u32 fpRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(FuncPtr));

    // arguments
    u32 argCount = 0;
    Datatype argType;
    if (parseType(&argType)) {
        argCount++;
        u32 argRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(Datatype));
        *(Datatype*)(&g_Unit->funcPtrTypes->bytes[argRef]) = argType;

        while (tok(Tok_Comma)) {
            argCount++;
            argType = expectType();
            argRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(Datatype));
            *(Datatype*)(&g_Unit->funcPtrTypes->bytes[argRef]) = argType;
        }
    }

    FuncPtr* funcPtr = getFuncPtr(fpRef);
    funcPtr->returnType = retType;
    funcPtr->argCount = argCount;

    expect(Tok_CloseParen);

    Datatype funcType;
    funcType.kind = Typekind_FuncPtr;
    funcType.ref = fpRef;
    funcType.numPointers = 1;

    // look if the function pointer is a duplicate
    u32 i = 0;
    while (i < oldLength) {
        FuncPtr* f = getFuncPtr(i);

        if (funcPtrEquivalence(f, funcPtr)) {
            // is duplicate
            g_Unit->funcPtrTypes->length = oldLength; // reset
            funcType.ref = i;
            return funcType;
        }

        i += sizeof(FuncPtr) + sizeof(Datatype) * f->argCount;
    }

    return funcType;
}

// returns false if the token can not be interpreted as a type
static bool parseInferableType(Datatype* type) {
    if (tok(Tok_Keyword_Let)) {
        *type = (Datatype){ Typekind_MustBeInfered, 0, 0 };
        return true;
    }

    if (tokens[token_index].type == Tok_Word) {
        u32 stbo = tokens[token_index++].stringTableByteOffset;

        type->ref = 0;

             if (stbo == type_name_int8) type->kind = Typekind_int8;
        else if (stbo == type_name_uint8) type->kind = Typekind_uint8;
        else if (stbo == type_name_int16) type->kind = Typekind_int16;
        else if (stbo == type_name_uint16) type->kind = Typekind_uint16;
        else if (stbo == type_name_int32) type->kind = Typekind_int32;
        else if (stbo == type_name_uint32) type->kind = Typekind_uint32;
        else if (stbo == type_name_int64) type->kind = Typekind_int64;
        else if (stbo == type_name_uint64) type->kind = Typekind_uint64;
        else if (stbo == type_name_float32) type->kind = Typekind_float32;
        else if (stbo == type_name_float64) type->kind = Typekind_float64;
        else if (stbo == type_name_char) type->kind = Typekind_char;
        else if (stbo == type_name_void) type->kind = Typekind_void;
        else {
            type->kind = Typekind_Undecided;
            type->ref = stbo;
        }


        type->numPointers = 0;
        while (tok(Tok_Mul)) type->numPointers++;

        *type = parseFuncPtrArgs(*type);

        return true;
    }

    return false;
}

// returns false if the token can not be interpreted as a type
static inline bool parseType(Datatype* type) {
    if (parseInferableType(type)) {
        if (type->kind == Typekind_MustBeInfered) {
            error("Type cannot be infered here.");
        }
        return true;
    }

    return false;
}

static inline Datatype expectInferableType() {
    Datatype res = {0};
    if (!parseInferableType(&res)) {
        // error("Expected type, but got \"%.*s\" instead.",
            // tokens[token_index].value.length,
            // tokens[token_index].value.start);
        error("Expected type.");
    }
    return res;
}

static inline Datatype expectType() {
    Datatype res = expectInferableType();
    if (res.kind == Typekind_MustBeInfered) error("Type cannot be infered here.");
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

static void expectFuncCallArgs(FuncCall* func, Expression* funcExpr) {
    func->funcExpr = funcExpr;

    Expression* expr = parseExpression();
    if (expr) {
        func->args = darrayCreate(Expression*);
        darrayAdd(func->args, expr);

        while (tok(Tok_Comma)) {
            expr = expectExpression();
            darrayAdd(func->args, expr);
        }
    } else {
        func->args = null;
    }

    expect(Tok_CloseParen);
}



static Expression* basicLiteral(ExprType type) {
    LiteralExpression* lit = malloc(sizeof(LiteralExpression));
    lit->base.expressionType = type;
    lit->base.nodebase.lineNumber = tokens[token_index++].line;
    return (Expression*)lit;
}

static LiteralExpression* createLiteral(ExprType type) {
    LiteralExpression* lit = malloc(sizeof(LiteralExpression));
    lit->base.nodebase.filepath = g_Filename;
    lit->base.expressionType = type;
    lit->base.nodebase.lineNumber = tokens[token_index].line;
    return lit;
}

static UnaryExpression* createUnaryExpr(ExprType type) {
    UnaryExpression* unary = malloc(sizeof(UnaryExpression));
    unary->base.expressionType = type;
    unary->base.nodebase.lineNumber = tokens[token_index++].line;
    return unary;
}

static Expression* parseLeafExpression() {
    Expression* res = null;

    UnaryExpression* unary = null;
    switch (tokens[token_index].type) {
        case Tok_Mul: unary = createUnaryExpr(ExprType_Unary_AddressOf); break;
        case Tok_At: unary = createUnaryExpr(ExprType_Unary_ValueOf); break;
        case Tok_ExclamationMark: unary = createUnaryExpr(ExprType_Unary_Not); break;
        case Tok_PlusPlus: unary = createUnaryExpr(ExprType_Unary_PreIncrement); break;
        case Tok_MinusMinus: unary = createUnaryExpr(ExprType_Unary_PreDecrement); break;
        case Tok_Minus: unary = createUnaryExpr(ExprType_Unary_Negate); break;
        case Tok_Tilde: unary = createUnaryExpr(ExprType_Unary_BitwiseNot); break;
        default: break;
    }

    switch (tokens[token_index].type) {
        case Tok_Word: {
            VariableExpression* ve = malloc(sizeof(VariableExpression));
            ve->base.expressionType = ExprType_Variable;
            ve->base.nodebase.lineNumber = tokens[token_index].line;
            ve->name = identifier();
            res = (Expression*)ve;
        } break;

        case Tok_Keyword_Alloc: {
            AllocExpression* alloc = malloc(sizeof(AllocExpression));
            alloc->base.expressionType = ExprType_Alloc;
            alloc->base.nodebase.lineNumber = tokens[token_index].line;

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
            SizeofExpression* sof = malloc(sizeof(SizeofExpression));
            sof->base.expressionType = ExprType_Sizeof;
            sof->base.nodebase.lineNumber = tokens[token_index].line;
            token_index++;

            bool mustClose = false;
            if (tok(Tok_OpenParen)) mustClose = true;
            sof->type = expectType();
            if (mustClose) if (!tok(Tok_CloseParen)) {
                error("Expected a closing parenthesis.");
            }

            res = (Expression*)sof;
        } break;

        case Tok_OpenParen: {
            ParenthesizedExpression* p = malloc(sizeof(ParenthesizedExpression));
            p->base.expressionType = ExprType_Parenthesized;
            p->base.nodebase.lineNumber = tokens[token_index++].line;
            p->innerExpr = expectExpression();
            expect(Tok_CloseParen);

            res = (Expression*)p;
        } break;


        case Tok_Integer: {
            LiteralExpression* lit = createLiteral(ExprType_Literal_Integer);
            lit->integer = tokens[token_index++].integer;
            res = (Expression*)lit;
        } break;
        case Tok_Decimal: {
            LiteralExpression* lit = createLiteral(ExprType_Literal_Decimal);
            lit->decimal = tokens[token_index++].decimal;
            res = (Expression*)lit;
        } break;
        case Tok_String: {
            LiteralExpression* lit = createLiteral(ExprType_Literal_String);
            lit->string = tokens[token_index++].stringTableByteOffset;
            res = (Expression*)lit;
        } break;
        case Tok_Char: {
            LiteralExpression* lit = createLiteral(ExprType_Literal_Char);
            lit->character = tokens[token_index++].character;
            res = (Expression*)lit;
        } break;

        case Tok_Keyword_True:  res = basicLiteral(ExprType_Literal_True); break;
        case Tok_Keyword_False: res = basicLiteral(ExprType_Literal_False); break;
        case Tok_Keyword_Null:  res = basicLiteral(ExprType_Literal_Null); break;

        default: return null;
    }

    while (true) {
        if (tok(Tok_Period)) {
            DerefOperator* deref = malloc(sizeof(DerefOperator));
            deref->base.expressionType = ExprType_Deref;
            deref->base.nodebase.lineNumber = tokens[token_index - 1].line;
            deref->expr = res;
            deref->derefOp = null;
            deref->name = identifier();
            res = (Expression*)deref;
        } else if (tok(Tok_OpenSquare)) {
            IndexingExpression* ind = malloc(sizeof(IndexingExpression));
            ind->base.expressionType = ExprType_Indexing;
            ind->base.nodebase.lineNumber = tokens[token_index - 1].line;
            ind->indexed = res;
            ind->index = expectExpression();
            expect(Tok_CloseSquare);
            res = (Expression*)ind;
        } else if (tok(Tok_OpenParen)) {
            FuncCall* call = malloc(sizeof(FuncCall));
            call->base.nodebase.filepath = g_Filename;
            call->base.expressionType = ExprType_FuncCall;
            expectFuncCallArgs(call, res);
            res = (Expression*)call;
        } else {
            break;
        }
    }

    if (unary) {
        unary->expr = res;
        res = (Expression*)unary;
    }

    if (tok(Tok_PlusPlus)) {
        UnaryExpression* unary = malloc(sizeof(UnaryExpression));
        unary->base.expressionType = ExprType_Unary_PostIncrement;
        unary->base.nodebase.lineNumber = tokens[token_index-1].line;
        unary->expr = res;
        res = (Expression*)unary;
    } else if (tok(Tok_MinusMinus)) {
        UnaryExpression* unary = malloc(sizeof(UnaryExpression));
        unary->base.expressionType = ExprType_Unary_PostDecrement;
        unary->base.nodebase.lineNumber = tokens[token_index-1].line;
        unary->expr = res;
        res = (Expression*)unary;
    }

    if (tok(Tok_Keyword_As)) {
        CastExpression* cast = malloc(sizeof(CastExpression));
        cast->base.expressionType = ExprType_Cast;
        cast->base.nodebase.lineNumber = tokens[token_index - 1].line;

        cast->expr = res;
        cast->castToType = expectType();
        res = (Expression*)cast;
    }

    return res;
}

static Expression* testForTernary(Expression* expr) {
    if (!tok(Tok_QuestionMark)) return expr;

    TernaryExpression* ter = malloc(sizeof(TernaryExpression));
    ter->base.expressionType = ExprType_Ternary;
    ter->base.nodebase.lineNumber = expr->nodebase.lineNumber;

    ter->condition = expr;
    ter->thenExpr = expectExpression();
    expect(Tok_Colon);
    ter->elseExpr = expectExpression();

    return (Expression*)ter;
}

static Expression* expectExpression() {
    Expression* res = parseExpression();
    if (res == null) {
        // error("Expected expression, but got \"%.*s\" instead.",
            // tokens[token_index].value.length,
            // tokens[token_index].value.start);

        error("Expected expression.");
        token_index++;
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

static Expression* expectLeafExpression() {
    Expression* res = parseLeafExpression();
    if (!res)
        // error("Expected expression, but got \"%.*s\" instead.",
            // tokens[token_index].value.length,
            // tokens[token_index].value.start);
        error("Expected expression.");
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
    token_index++;

    BinaryExpression* root = malloc(sizeof(BinaryExpression));
    root->base.expressionType = exprType;
    root->left = a;
    root->right = expectLeafExpression();

    tokentype = tokens[token_index].type;
    while ( (exprType = getExprTypeForBinaryOperator(tokentype)) ) {
        token_index++;

        BinaryExpression* op = malloc(sizeof(BinaryExpression));
        op->base.expressionType = exprType;
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

    expectBlock(&res->scope);

    if (tok(Tok_Keyword_Else)) {
        if (tok(Tok_Keyword_If)) {
            // TODO: line number
            res->next = expectIfStatement();
        } else {
            IfStatement* elseStatement = malloc(sizeof(IfStatement));
            elseStatement->base.statementType = Statement_If;
            elseStatement->condition = null;
            elseStatement->next = null;
            expectBlock(&elseStatement->scope);

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
    decl->type = expectInferableType();
    decl->name = identifier();

    decl->assignmentOrNull = null;
    if (tok(Tok_Assign)) {
        decl->assignmentOrNull = expectExpression();
    } else if (decl->type.kind == Typekind_MustBeInfered) {
        error("Variable \"%s\" must be assigned to, to be type inferred.", getIdentifierStringValue(decl->name));
    }

    return decl;
}

static Statement* expectStatement() {
    Statement* res = null;

    u32 startingLineNum = tokens[token_index].line;

    switch (tokens[token_index].type) {

        case Tok_OpenCurl: {
            Scope* scope = malloc(sizeof(Scope));
            scope->base.statementType = Statement_Scope;
            expectBlock(&scope->codeblock);

            res = (Statement*)scope;
        } break;
        case Tok_Keyword_While: {
            token_index++;

            WhileStatement* whileStatement = malloc(sizeof(WhileStatement));
            whileStatement->base.statementType = Statement_While;

            whileStatement->condition = expectExpression();

            expectBlock(&whileStatement->scope);

            res = (Statement*)whileStatement;
        } break;
        case Tok_Keyword_If: {
            token_index++;
            res = (Statement*)expectIfStatement();
        } break;


        case Tok_Keyword_Continue: {
            res = malloc(sizeof(Statement));
            res->statementType = Statement_Continue;
            token_index++;
            semicolon();
        } break;
        case Tok_Keyword_Break: {
            res = malloc(sizeof(Statement));
            res->statementType = Statement_Break;
            token_index++;
            semicolon();
        } break;
        case Tok_Keyword_Return: {
            res = malloc(sizeof(ReturnStatement));
            res->statementType = Statement_Return;
            token_index++;
            ((ReturnStatement*)res)->returnExpr = parseExpression();
            semicolon();
        } break;

        case Tok_Keyword_Goto: {
            GotoStatement* go = malloc(sizeof(GotoStatement));
            go->base.statementType = Statement_Goto;
            token_index++;
            go->label = identifier();
            res = (Statement*)go;
            semicolon();
        } break;

        case Tok_Keyword_Let: {
            res = (Statement*)expectVarDecl();
            semicolon();
        } break;

        default: {

            if (tokens[token_index].type == Tok_Word && tokens[token_index + 1].type == Tok_Colon) {
                LabelStatement* label = malloc(sizeof(LabelStatement));
                label->base.statementType = Statement_Label;
                label->label = tokens[token_index++].stringTableByteOffset;
                token_index++;
                res = (Statement*)label;
                break;
            }

            { // declaration
                u32 postType = peekType();
                if (postType && tokens[postType].type == Tok_Word) {
                    // confirmed declaration

                    VarDecl* decl = malloc(sizeof(VarDecl));
                    decl->base.statementType = Statement_Declaration;
                    decl->type = expectInferableType();
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
                    res = (Statement*)decl;
                    break;
                }
            }


            Expression* expr = parseExpression();
            if (expr) {
                switch (expr->expressionType) {
                    case ExprType_Unary_PreIncrement:
                    case ExprType_Unary_PostIncrement:
                    case ExprType_Unary_PreDecrement:
                    case ExprType_Unary_PostDecrement:
                    case ExprType_FuncCall: {
                        StatementExpression* staExpr = malloc(sizeof(StatementExpression));
                        staExpr->base.statementType = Statement_Expression;
                        staExpr->base.nodebase.lineNumber = expr->nodebase.lineNumber;
                        staExpr->expr = expr;
                        res = (Statement*)staExpr;
                    } break;
                    case ExprType_Indexing:
                    case ExprType_Unary_ValueOf:
                    case ExprType_Variable:
                    case ExprType_Deref: {

                        Token* token = anyof(5, Tok_Assign, Tok_PlusAssign, Tok_MinusAssign, Tok_MulAssign, Tok_DivAssign);
                        if (token) {
                            Assignement* ass = malloc(sizeof(Assignement));
                            ass->base.statementType = Statement_Assignment;
                            ass->assigneeExpr = expr;
                            ass->assignmentOper = token->type;
                            ass->expr = expectExpression();
                            res = (Statement*)ass;
                        } else {
                            error("Expected an assignment.");
                        }

                    } break;
                    default:
                        error("This expression is all by its lonesome.");
                        return null;
                }
                semicolon();
                break;
            }

            unexpectedToken();
            return null;
        }
    }

    // res should never be null up to this point
    res->nodebase.lineNumber = startingLineNum;

    return res;
}

static void expectBlock(Codeblock* scope) {
    expect(Tok_OpenCurl);

    scope->statements = darrayCreate(Statement*);

    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            darrayAdd(scope->statements, statement);
        } else {
            // there must have been an error.
        }
    }
}

static PlangStruct expectStruct() {
    PlangStruct stru;
    stru.name = identifier();
    stru.fields = darrayCreate(Field);

    expect(Tok_OpenCurl);

    do {
        Field field;
        field.nodebase.lineNumber = tokens[token_index].line;
        field.type = expectType();
        field.name = identifier();
        semicolon();

        darrayAdd(stru.fields, field);

    } while (tokens[token_index].type != Tok_CloseCurl);

    token_index++;

    return stru;
}

static FuncArg* expectFuncArgList() {
    FuncArg* res = null;

    FuncArg arg;
    if (parseType(&arg.type)) {
        arg.name = identifier();

        res = darrayCreate(FuncArg);
        darrayAdd(res, arg);

        while (tok(Tok_Comma)) {
            arg.type = expectType();
            arg.name = identifier();
            darrayAdd(res, arg);
        }
    }

    expect(Tok_CloseParen);

    return res;
}

static void funcOrGlobal() {
    Datatype type = expectInferableType();
    Identifier name = identifier();

    if (tok(Tok_OpenParen)) {
        // function

        PlangFunction func;
        func.overload = 0;
        func.decl.returnType = type;
        func.decl.name = name;
        func.decl.arguments = expectFuncArgList();

        expectBlock(&func.scope);
        darrayAdd(g_Unit->functions, func);

    } else {
        // global variable
        VarDecl decl;
        decl.base.statementType = Statement_Declaration;
        decl.assignmentOrNull = null;
        decl.name = name;
        decl.type = type;

        if (tok(Tok_Assign)) {
            decl.assignmentOrNull = expectExpression();
        } else if (decl.type.kind == Typekind_MustBeInfered) {
            error("Global variable \"%s\" must be assigned to, to be type inferred.", getIdentifierStringValue(decl.name));
        }

        darrayAdd(g_Unit->globalVariables, decl);

        semicolon();
    }
}

static u32 parse() {


    // token_index = 0;

    u32 tokens_length = darrayLength(tokens);

    while (token_index < tokens_length) {

        switch (tokens[token_index].type) {
            case Tok_Keyword_Struct: {
                // struct
                u32 lineNum = tokens[token_index].line;
                token_index++;
                PlangStruct stru = expectStruct();
                stru.nodebase.lineNumber = lineNum;
                u32 struLen = darrayLength(g_Unit->structs);
                for (u32 i = 0; i < struLen; i++) {
                    if (g_Unit->structs[i].name == stru.name) {
                        error_line(stru.nodebase.lineNumber, "Struct \"%s\" is already defined.", getIdentifierStringValue(stru.name));
                        break;
                    }
                }

                darrayAdd(g_Unit->structs, stru);

            } break;

            case Tok_Keyword_Type: {
                u32 lineNum = tokens[token_index].line;
                token_index++;

                AliasType alias;
                alias.name = identifier();
                if (tok(Tok_Assign)) {
                    alias.aliasedType = expectType();
                    darrayAdd(g_Unit->aliases, alias);
                } else {
                    darrayAdd(g_Unit->opaqueTypes, alias.name);
                }
                semicolon();

            } break;

            case Tok_Keyword_Const: {
                u32 lineNum = tokens[token_index].line;
                token_index++;

                Constant constant;
                constant.name = identifier();
                expect(Tok_Assign);
                constant.expr = expectExpression();
                semicolon();

                darrayAdd(g_Unit->constants, constant);
            } break;

            case Tok_Keyword_Let:
            case Tok_Word: funcOrGlobal(); break;

            case Tok_Keyword_Declare: {
                // function declaration
                token_index++;
                FuncDeclaration funcDecl;
                funcDecl.returnType = expectType();
                funcDecl.name = identifier();
                expect(Tok_OpenParen);
                funcDecl.arguments = expectFuncArgList();
                semicolon();

                darrayAdd(g_Unit->functionDeclarations, funcDecl);
            } break;

            default: {
                unexpectedToken();
            } break;
        }
    }

    return numberOfErrors;
}
