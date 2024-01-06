

static void expectProcCallArgs(Parser* parser, ProcCall* proc, Expression* proc_expr) {
    proc->proc_expr = proc_expr;

    Expression* expr = parseExpression(parser);
    if (expr) {
        proc->args = list_create(Expression*);
        list_add(proc->args, expr);

        while (tok(parser, Tok_Comma)) {
            expr = expectExpression(parser);
            list_add(proc->args, expr);
        }
    } else {
        proc->args = null;
    }

    expect(parser, Tok_CloseParen);
}

static Expression* literal_expr(Parser* parser, ExprType expr_type) {
    LiteralExpression* lit = allocExpr(parser, expr_type);
    lit->data = advance(parser).data;
    return (Expression*)lit;
}

static Expression* parseLeafExpression(Parser* parser) {
    Expression* res = null;

    UnaryExpression* unary = null;
    switch (peek(parser).type) {
        case Tok_Mul: unary = allocExpr(parser, ExprType_Unary_AddressOf); advance(parser); break;
        case Tok_At: unary = allocExpr(parser, ExprType_Unary_ValueOf); advance(parser); break;
        case Tok_ExclamationMark: unary = allocExpr(parser, ExprType_Unary_Not); advance(parser); break;
        case Tok_PlusPlus: unary = allocExpr(parser, ExprType_Unary_PreIncrement); advance(parser); break;
        case Tok_MinusMinus: unary = allocExpr(parser, ExprType_Unary_PreDecrement); advance(parser); break;
        case Tok_Minus: unary = allocExpr(parser, ExprType_Unary_Negate); advance(parser); break;
        case Tok_Tilde: unary = allocExpr(parser, ExprType_Unary_BitwiseNot); advance(parser); break;
        default: break;
    }

    switch (peek(parser).type) {
        case Tok_Word: {
            VariableExpression* ve = allocExpr(parser, ExprType_Variable);
            ve->name = advance(parser).data.string;

            ve->ref = get_symbol(parser, ve->name);
            if (!ve->ref) list_add(parser->unresolved_variables, ve);

            res = (Expression*)ve;
        } break;

        case Tok_Keyword_Alloc: {
            AllocExpression* alloc = allocExpr(parser, ExprType_Alloc);
            advance(parser);
            alloc->type = expectType(parser);
            alloc->sizeExpr = null;
            if (tok(parser, Tok_OpenSquare)) {
                alloc->sizeExpr = expectExpression(parser);
                expect(parser, Tok_CloseSquare);
            }

            res = (Expression*)alloc;
        } break;

        case Tok_Keyword_Sizeof: {
            SizeofExpression* sof = allocExpr(parser, ExprType_Sizeof);
            advance(parser);

            bool mustClose = false;
            if (tok(parser, Tok_OpenParen)) mustClose = true;
            sof->type = expectType(parser);
            if (mustClose) expect(parser, Tok_CloseParen);

            res = (Expression*)sof;
        } break;

        case Tok_OpenParen: {
            ParenthesizedExpression* p = allocExpr(parser, ExprType_Parenthesized);
            advance(parser);
            p->innerExpr = expectExpression(parser);
            expect(parser, Tok_CloseParen);

            res = (Expression*)p;
        } break;

        case Tok_Period: {
            DerefOperator* deref = allocExpr(parser, ExprType_Deref);
            advance(parser);
            deref->name = identifier(parser);
            res = (Expression*)deref;
        } break;

        case Tok_OpenCurl: {
            CompoundExpression* com = allocExpr(parser, ExprType_Compound);
            advance(parser);
            res = (Expression*)com;

            if (tok(parser, Tok_CloseCurl)) break;

            com->elements = list_create(CompoundElement);
            do {
                CompoundElement el = {0};
                if (peek_at(parser, 1).type == Tok_Assign) {
                    el.name = identifier(parser);
                    advance(parser);
                }

                el.expr = expectExpression(parser);
                list_add(com->elements, el);

            } while (tok(parser, Tok_Comma));

            expect(parser, Tok_CloseCurl);
        } break;

        case Tok_Integer:       res = literal_expr(parser, ExprType_Literal_Integer); break;
        case Tok_Decimal:       res = literal_expr(parser, ExprType_Literal_Decimal); break;
        case Tok_String:        res = literal_expr(parser, ExprType_Literal_String); break;
        case Tok_Char:          res = literal_expr(parser, ExprType_Literal_Char); break;
        case Tok_Keyword_True:  res = allocExpr(parser, ExprType_Literal_True); advance(parser); break;
        case Tok_Keyword_False: res = allocExpr(parser, ExprType_Literal_False); advance(parser); break;
        case Tok_Keyword_Null:  res = allocExpr(parser, ExprType_Literal_Null); advance(parser); break;

        default: return null;
    }

    while (true) {
        switch (peek(parser).type) {
            case Tok_Period: {
                DerefOperator* deref = allocExpr(parser, ExprType_Deref);
                advance(parser);

                deref->expr = res;
                deref->name = identifier(parser);
                res = (Expression*)deref;
            } continue;

            case Tok_OpenSquare: {
                IndexingExpression* ind = allocExpr(parser, ExprType_Indexing);
                advance(parser);

                ind->indexed = res;
                ind->index = expectExpression(parser);
                expect(parser, Tok_CloseSquare);
                res = (Expression*)ind;
            } continue;

            case Tok_OpenParen: {
                ProcCall* call = allocExpr(parser, ExprType_ProcCall);
                advance(parser);

                expectProcCallArgs(parser, call, res);
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

    switch (peek(parser).type) {
        case Tok_PlusPlus: {
            UnaryExpression* postInc = allocExpr(parser, ExprType_Unary_PostIncrement);
            advance(parser);
            postInc->expr = res;
            res = (Expression*)postInc;
        } break;

        case Tok_MinusMinus: {
            UnaryExpression* postDec = allocExpr(parser, ExprType_Unary_PostDecrement);
            advance(parser);
            postDec->expr = res;
            res = (Expression*)postDec;
        } break;

        default: break;
    }

    if (peek(parser).type == Tok_Keyword_As) {
        CastExpression* cast = allocExpr(parser, ExprType_Cast);
        advance(parser);
        cast->expr = res;
        cast->castToType = expectType(parser);
        res = (Expression*)cast;
    }

    return res;
}

static Expression* testForTernary(Parser* parser, Expression* expr) {
    if (peek(parser).type != Tok_QuestionMark) return expr;

    TernaryExpression* ter = allocExpr(parser, ExprType_Ternary);
    advance(parser);

    ter->condition = expr;
    ter->thenExpr = expectExpression(parser);
    expect(parser, Tok_Colon);
    ter->elseExpr = expectExpression(parser);

    return (Expression*)ter;
}

static Expression* expectExpression(Parser* parser) {
    Expression* res = parseExpression(parser);
    if (res == null) fatal_parse_error(parser, "Expected expression.");
    return res;
}

static u32 operatorPriority(ExprType type) {
    switch (type) {

        case ExprType_BooleanAnd:
        case ExprType_BooleanOr:
            return 1;

        case ExprType_Less:
        case ExprType_Greater:
        case ExprType_LessEquals:
        case ExprType_GreaterEquals:
        case ExprType_Equals:
        case ExprType_NotEquals:
            return 2;

        case ExprType_Bitwise_Lshift:
        case ExprType_Bitwise_Rshift:
        case ExprType_Bitwise_And:
        case ExprType_Bitwise_Or:
        case ExprType_Bitwise_Xor:
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

static Expression* expectLeafExpression(Parser* parser) {
    Expression* res = parseLeafExpression(parser);
    if (!res) fatal_parse_error(parser, "Expected expression.");
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

static Expression* parseExpression(Parser* parser) {
    Expression* a = parseLeafExpression(parser);
    if (!a) return null;

    TokenType tokentype = peek(parser).type;
    ExprType exprType = getExprTypeForBinaryOperator(tokentype);
    if (!exprType) return testForTernary(parser, a);

    BinaryExpression* root = allocExpr(parser, exprType);
    advance(parser);
    root->left = a;
    root->right = expectLeafExpression(parser);

    tokentype = peek(parser).type;
    while ( (exprType = getExprTypeForBinaryOperator(tokentype)) ) {

        BinaryExpression* op = allocExpr(parser, exprType);
        advance(parser);
        op->right = expectLeafExpression(parser);

        root = appendBinaryExpression(root, op);

        tokentype = peek(parser).type;
    }

    return testForTernary(parser, (Expression*)root);
}
