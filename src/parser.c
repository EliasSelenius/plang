

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
    return token->string;
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


static inline bool funcPtrEquivalence(ProcPtr* a, ProcPtr* b) {
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

// returns false if the token can not be interpreted as a type
static bool parseInferableType(Datatype* type) {
    if (tok(Tok_Keyword_Let)) {
        *type = (Datatype){ Typekind_MustBeInfered, 0, 0 };
        return true;
    }

    if (tokens[token_index].type == Tok_Word) {
        u32 stbo = tokens[token_index++].string;

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

        *type = parseProcPtrArgs(*type);

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
            ve->namespace_name = 0;
            ve->name = identifier();
            if (tokens[token_index].type == Tok_Colon
             && tokens[token_index + 1].type == Tok_Colon) {
                token_index += 2;
                ve->namespace_name = ve->name;
                ve->name = identifier();
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
    decl->type = expectInferableType();
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
    ProcArg* res = null;

    ProcArg arg;
    if (parseType(&arg.type)) {
        arg.name = identifier();

        res = list_create(ProcArg);
        list_add(res, arg);

        while (tok(Tok_Comma)) {
            arg.type = expectType();
            arg.name = identifier();
            list_add(res, arg);
        }
    }

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
            localproc->proc.returnType = expectInferableType();
            localproc->proc.name = identifier();
            expect(Tok_OpenParen);
            localproc->proc.arguments = expectProcArguments();
            localproc->proc.scope = expectScope();

            return (Statement*)localproc;
        } else {
            // confirmed declaration

            VarDecl* decl = allocStatement(Statement_Declaration);
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

static void funcOrGlobal() {
    Datatype type = expectInferableType();
    Identifier name = identifier();

    if (tok(Tok_OpenParen)) {
        // function

        Procedure proc;
        proc.overload = 0;
        proc.returnType = type;
        proc.name = name;
        proc.arguments = expectProcArguments();

        activateScope(null);
        proc.scope = expectScope();

        list_add(context->namespace->procedures, proc);

    } else {
        // global variable
        VarDecl decl;
        decl.base.statementType = Statement_Declaration;
        decl.assignmentOrNull = null;
        decl.name = name;
        decl.type = type;

        globvar:
        if (tok(Tok_Assign)) {
            decl.assignmentOrNull = expectExpression();
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

            AliasType alias;
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

            Constant constant;
            constant.name = identifier();
            expect(Tok_Assign);
            constant.expr = expectExpression();
            semicolon();

            list_add(context->namespace->constants, constant);
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
        Namespace* ns = getNamespace(name);
        context->namespace = ns;
        semicolon();
    } else context->namespace = g_Codebase.namespaces[0];

    while (parseProgramEntity());
}

static bool tryParseThing() {
    switch (tokens[token_index].type) {
        case Tok_Keyword_Struct: {
            PlangStruct stru = {0};
            stru.nodebase.lineNumber = tokens[token_index].line;
            stru.nodebase.filepath = context->filename;
            token_index++;
            stru.name = identifier();

            list_add(context->namespace->structs, stru);
        } return true;

        case Tok_Keyword_Type: return true;
        case Tok_Keyword_Const: return true;
        case Tok_EOF: return false;
        default: return true;
    }
}

static void parseAllTypes() {
    u32 tokens_length = list_length(tokens);
    u32 file_index = 0;
    token_index = 0;
    while (token_index < tokens_length) {
        context = &Files[file_index++];

        if (tok(Tok_Keyword_Namespace)) {
            Identifier name = identifier();
            Namespace* ns = getNamespace(name);
            context->namespace = ns;
            semicolon();
        } else context->namespace = g_Codebase.namespaces[0];

        while (tryParseThing());
    }
}

static void parse() {

    u32 tokens_length = list_length(tokens);
    u32 file_index = 0;
    token_index = 0;
    while (token_index < tokens_length) {
        context = &Files[file_index++];
        parseFile();
    }
}
