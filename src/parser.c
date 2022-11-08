

static Scope* g_CurrentScope;



static Scope* expectScope();
static Expression* parseExpression();
static Expression* expectExpression();
static bool parseType(Datatype* type);
static Datatype expectType();
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
    return token->stringTableByteOffset;
}

static void expect(TokenType type) {
    if (tokens[token_index].type != type) {
        // TODO: get TokenType as string
        error_token("Unexpected token type %d.", tokens[token_index].type);
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


static Datatype ensureFuncPtrExistsFromProcedure(Procedure* proc) {

    u32 oldLength = g_Unit->funcPtrTypes->length; // remember the old length in case we have a duplicate
    u32 fpRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(FuncPtr));

    u32 argCount = proc->arguments ? darrayLength(proc->arguments) : 0;
    for (u32 i = 0; i < argCount; i++) {
        u32 argRef = dyReserve(&g_Unit->funcPtrTypes, sizeof(Datatype));
        *(Datatype*)(&g_Unit->funcPtrTypes->bytes[argRef]) = proc->arguments[i].type;
    }

    FuncPtr* funcPtr = getFuncPtr(fpRef);
    funcPtr->returnType = proc->returnType;
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
            error_at_token(token_index - 1, "Type cannot be infered here.");
        }
        return true;
    }

    return false;
}

static inline Datatype expectInferableType() {
    Datatype res = {0};
    if (!parseInferableType(&res)) {
        error_token("Expected type.");
    }
    return res;
}

static inline Datatype expectType() {
    Datatype res = expectInferableType();
    if (res.kind == Typekind_MustBeInfered) error_at_token(token_index - 1, "Type cannot be infered here.");
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
            ve->name = identifier();
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
            lit->string = tokens[token_index++].stringTableByteOffset;
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
                deref->derefOp = null;
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
                FuncCall* call = allocExpr(ExprType_FuncCall);
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
    decl->type = expectInferableType();
    decl->name = identifier();

    decl->assignmentOrNull = null;
    if (tok(Tok_Assign)) {
        decl->assignmentOrNull = expectExpression();
    } else if (decl->type.kind == Typekind_MustBeInfered) {
        error_token("Variable \"%s\" must be assigned to, to be type inferred.", getIdentifierStringValue(decl->name));
    }

    return decl;
}

static Statement* expectStatement() {
    Statement* res = null;

    u32 startingLineNum = tokens[token_index].line;

    switch (tokens[token_index].type) {

        case Tok_OpenCurl: {
            res = (Statement*)expectScope();
        } break;

        case Tok_Keyword_While: {
            token_index++;

            WhileStatement* whileStatement = malloc(sizeof(WhileStatement));
            whileStatement->base.statementType = Statement_While;
            whileStatement->condition = expectExpression();
            whileStatement->statement = expectStatement();

            res = (Statement*)whileStatement;
        } break;
        case Tok_Keyword_If: {
            token_index++;
            res = (Statement*)expectIfStatement();
        } break;
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
            res = (Statement*)forin;
        } break;

        case Tok_Keyword_Switch: {
            SwitchStatement* switchStatement = allocStatement(Statement_Switch);
            token_index++;

            switchStatement->expr = expectExpression();
            switchStatement->scope = expectScope();

            res = (Statement*)switchStatement;
        } break;
        case Tok_Keyword_Case: {
            token_index++;

            CaseLabelStatement* caseLabel = malloc(sizeof(CaseLabelStatement));
            caseLabel->base.statementType = Statement_CaseLabel;
            caseLabel->expr = expectExpression();
            expect(Tok_Colon);

            res = (Statement*)caseLabel;
        } break;
        case Tok_Keyword_Default: {
            res = malloc(sizeof(Statement));
            res->statementType = Statement_DefaultLabel;
            token_index++;
            expect(Tok_Colon);
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


            { // expression statement
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

                        Token* token = anyof(8, Tok_Assign,
                                                Tok_PlusAssign,
                                                Tok_MinusAssign,
                                                Tok_MulAssign,
                                                Tok_DivAssign,
                                                Tok_BitAndAssign,
                                                Tok_BitOrAssign,
                                                Tok_BitXorAssign);
                        if (token) {
                            Assignement* ass = malloc(sizeof(Assignement));
                            ass->base.statementType = Statement_Assignment;
                            ass->assigneeExpr = expr;
                            ass->assignmentOper = token->type;
                            ass->expr = expectExpression();
                            res = (Statement*)ass;
                        } else {
                            error_token("Expected an assignment.");
                        }

                    } break;
                    default:
                        error_token("This expression is all by its lonesome.");
                        return null;
                }

                semicolon();
                break;
            }
        }
    }

    // res should never be null up to this point
    res->nodebase.lineNumber = startingLineNum;

    return res;
}

static Scope* expectScope() {

    Scope* scope = allocStatement(Statement_Scope);
    scope->parentScope = g_CurrentScope;
    scope->statements = darrayCreate(Statement*);

    expect(Tok_OpenCurl);

    g_CurrentScope = scope;

    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            darrayAdd(scope->statements, statement);
        } else {
            // there must have been an error.
        }
    }

    g_CurrentScope = scope->parentScope;
    return scope;
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
        darrayAdd(stru.fields, field);

        while (tok(Tok_Comma)) {
            field.nodebase.lineNumber = tokens[token_index].line;
            field.name = identifier();
            darrayAdd(stru.fields, field);
        }

        semicolon();
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

        Procedure proc;
        proc.overload = 0;
        proc.returnType = type;
        proc.name = name;
        proc.arguments = expectFuncArgList();

        g_CurrentScope = null;
        proc.scope = expectScope();

        darrayAdd(g_Unit->procedures, proc);

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
            error_token("Global variable \"%s\" must be assigned to, to be type inferred.", getIdentifierStringValue(decl.name));
        }

        darrayAdd(g_Unit->globalVariables, decl);

        semicolon();
    }
}

static void parse() {


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
                        error_node(&stru, "Struct \"%s\" is already defined.", getIdentifierStringValue(stru.name));
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

                Procedure proc;
                proc.overload = 0;
                proc.scope = null;
                proc.returnType = expectType();
                proc.name = identifier();
                expect(Tok_OpenParen);
                proc.arguments = expectFuncArgList();
                semicolon();

                darrayAdd(g_Unit->procedures, proc);
            } break;

            default: {
                unexpectedToken();
            } break;
        }
    }
}
