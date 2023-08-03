
static bool isBasicType();
static bool _isBasicType_validModifier() {

    u32 ti = token_index;

    if (tok(Tok_Mul)) return true;

    if (tok(Tok_OpenParen)) {
        if (tok(Tok_CloseParen)) return true;

        do {
            if (!isBasicType()) goto nope;
        } while (tok(Tok_Comma));

        if (tok(Tok_CloseParen)) return true;
    }

    nope:
    token_index = ti;
    return false;
}

static bool isBasicType() {
    if (tok(Tok_Word)) {
        if (tok(Tok_Period)) if (!tok(Tok_Word)) return false;
        while (_isBasicType_validModifier());
        return true;
    }
    return false;
}

static u32 peekType() {

    if (tokens[token_index].type == Tok_Keyword_Let) return token_index + 1;

    u32 ti = token_index;
    u32 res = 0;
    if (isBasicType()) res = token_index;
    token_index = ti;
    return res;
}


static Statement* expectStatement() {
    switch (tokens[token_index].type) {

        case Tok_OpenCurl: return (Statement*)expectScope();

        case Tok_Keyword_While: {
            WhileStatement* whileStatement = allocStatement(Statement_While);
            token_index++;

            whileStatement->condition = expectExpression();
            whileStatement->statement = tok(Tok_Semicolon) ? null : expectStatement();

            return (Statement*)whileStatement;
        }

        case Tok_Keyword_If: {
            IfStatement* res = allocStatement(Statement_If);
            token_index++;

            res->condition = expectExpression();
            res->then_statement = tok(Tok_Semicolon) ? null : expectStatement();
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

        {
            static SwitchStatement* current_switch = null;

            case Tok_Keyword_Switch: {
                SwitchStatement* switchStatement = allocStatement(Statement_Switch);
                token_index++;

                switchStatement->expr = expectExpression();

                SwitchStatement* temp = current_switch;
                current_switch = switchStatement;
                switchStatement->scope = expectScope();
                current_switch = temp;

                return (Statement*)switchStatement;
            }

            case Tok_Keyword_Case: {
                CaseLabelStatement* caseLabel = allocStatement(Statement_CaseLabel);
                token_index++;
                caseLabel->expr = expectExpression();
                caseLabel->switch_statement = current_switch;
                expect(Tok_Colon);
                return (Statement*)caseLabel;
            }
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

        case Tok_Keyword_Const: {
            Declaration* decl = allocStatement(Statement_Constant);
            *decl = expectConst();
            return (Statement*)decl;
        }

        case Tok_Keyword_Struct: {
            Struct* sp = allocStatement(Statement_Struct);
            *sp = expectStruct();
            declare_local_type((Statement*)sp);
            return (Statement*)sp;
        }

        case Tok_Keyword_Enum: {
            Enum* en = allocStatement(Statement_Enum);
            expectEnum(en);
            declare_local_type((Statement*)en);
            return (Statement*)en;
        }

        case Tok_Keyword_Type: {
            Typedef* def = allocStatement(Statement_Typedef);
            *def = expectTypedef();
            declare_local_type((Statement*)def);
            return (Statement*)def;
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

        Node node = node_init();
        Type* type = expectType();
        Identifier name = identifier();

        if (tok(Tok_OpenParen)) {

            Procedure* proc = allocStatement(Statement_Procedure);
            proc->base.nodebase = node;
            proc->returnType = type;
            proc->name = name;
            proc->arguments = expectProcArguments();
            proc->scope = expectScope();

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
        } else {
            if (tok(Tok_Assign)) decl->expr = expectExpression();
        }

        semicolon();
        return (Statement*)decl;
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

    u32 local_types_count = list_length(parser.local_types);

    expect(Tok_OpenCurl);
    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            list_add(scope->statements, statement);
        } else {
            // there must have been an error.
        }
    }

    list_head(parser.local_types)->length = local_types_count;

    parser.scope = scope->parentScope;
    return scope;
}
