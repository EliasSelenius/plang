
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
    VarDecl* decl = allocStatement(Statement_Declaration);
    decl->type = expectType();
    decl->name = identifier();

    decl->assignmentOrNull = null;
    if (tok(Tok_Assign)) {
        decl->assignmentOrNull = expectExpression();
    } else if (decl->type->node_type == TypeNode_MustInfer) {
        error_token("Variable \"%s\" must be assigned to, to be type inferred.", get_string(decl->name));
    }

    return decl;
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
            IfStatement* res = allocStatement(Statement_If);
            token_index++;

            res->condition = expectExpression();
            res->then_statement = expectStatement();
            if (tok(Tok_Keyword_Else)) res->else_statement = expectStatement();

            return (Statement*)res;
        }

        case Tok_Keyword_For: {
            ForStatement* forsta = allocStatement(Statement_For);
            token_index++;

            if (tokens[token_index + 1].type == Tok_Keyword_In) {
                forsta->index_name = identifier();
                token_index++;
            } else if (tokens[peekType() + 1].type == Tok_Keyword_In) {
                forsta->index_type = expectType();
                forsta->index_name = identifier();
                token_index++;
            }

            forsta->min_expr = expectExpression();

            if (tok(Tok_Dotdot)) {
                forsta->max_expr = expectExpression();
            }

            forsta->statement = expectStatement();
            return (Statement*)forsta;
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
                // decl->type.numPointers++;
                // TODO: line above breaks fixed sized arrays
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
                Assignment* ass = allocStatement(Statement_Assignment);
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


static Scope* expectScope() {
    Scope* scope = allocStatement(Statement_Scope);
    scope->statements = list_create(Statement*);

    scope->parentScope = parser.scope;
    parser.scope = scope;

    expect(Tok_OpenCurl);
    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            list_add(scope->statements, statement);
        } else {
            // there must have been an error.
        }
    }

    parser.scope = scope->parentScope;
    return scope;
}
