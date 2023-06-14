

static void expectProcCallArgs(ProcCall* proc, Expression* proc_expr) {
    proc->proc_expr = proc_expr;

    Expression* expr = parseExpression();
    if (expr) {
        proc->args = list_create(Expression*);
        list_add(proc->args, expr);

        while (tok(Tok_Comma)) {
            expr = expectExpression();
            list_add(proc->args, expr);
        }
    } else {
        proc->args = null;
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
            ve->name = tokens[token_index++].string;
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

        case Tok_Period: {
            DerefOperator* deref = allocExpr(ExprType_Deref);
            token_index++;
            deref->name = identifier();
            res = (Expression*)deref;
        } break;

        case Tok_OpenCurl: {
            CompoundExpression* com = allocExpr(ExprType_Compound);
            token_index++;
            res = (Expression*)com;

            if (tok(Tok_CloseCurl)) break;

            com->elements = list_create(CompoundElement);
            do {
                CompoundElement el = {0};
                if (tokens[token_index + 1].type == Tok_Assign) {
                    el.name = identifier();
                    token_index++;
                }

                el.expr = expectExpression();
                list_add(com->elements, el);

            } while (tok(Tok_Comma));

            expect(Tok_CloseCurl);
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

                expectProcCallArgs(call, res);
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
