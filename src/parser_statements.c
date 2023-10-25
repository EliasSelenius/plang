
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
        else goto nope;
    }

    if (tok(Tok_OpenSquare)) {
        if (tok(Tok_CloseSquare)) return true;
        if (tok(Tok_Dotdot) && tok(Tok_CloseSquare)) return true;
        if (tok(Tok_Integer) && tok(Tok_CloseSquare)) return true;
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

            if (tokens[token_index + 1].type == Tok_Colon) {
                forsta->index_name = identifier();
                token_index++;
            } else if (tokens[peekType() + 1].type == Tok_Colon) {
                forsta->index_type = expectType();
                forsta->index_name = identifier();
                token_index++;
            } else {
                forsta->index_name = builtin_string_it;
            }

            forsta->min_expr = expectExpression();

            if (tok(Tok_Dotdot)) {
                forsta->max_expr = expectExpression();
            }

            declare_symbol((Statement*)forsta);
            forsta->statement = expectStatement();
            stack_pop(1);
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

        case Tok_Keyword_Struct: return expectStruct();
        case Tok_Keyword_Enum: return expectEnum();
        case Tok_Keyword_Type: return expectTypedef();
        case Tok_Keyword_Const: return expectConst();

        case Tok_Keyword_Static: {
            token_index++;
            Declaration* decl = allocStatement(Statement_Declaration);
            decl->type = expectType();
            decl->name = identifier();
            decl->is_static = true;
            if (tok(Tok_Assign)) decl->expr = expectExpression();
            semicolon();
            return (Statement*)decl;
        }

        default: break;
    }

    // label
    if (tokens[token_index].type == Tok_Word && tokens[token_index + 1].type == Tok_Colon) {
        LabelStatement* label = allocStatement(Statement_Label);
        label->label = tokens[token_index++].data.string;
        token_index++;
        return (Statement*)label;
    }

    // declaration or procedure
    u32 postType = peekType();
    if (postType && tokens[postType].type == Tok_Word) {
        return proc_or_var(true);
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
    u32 local_symbols_count = list_length(parser.local_symbols);

    expect(Tok_OpenCurl);
    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            list_add(scope->statements, statement);

            switch (statement->statementType) {
                // case Statement_For: // declare_symbol() is called in expectStatement() for for-loops, because it must be called before we parse inner statement
                // case Statement_Procedure: // declare_symbol() is called in proc_or_var() for same reason as above

                case Statement_FixedArray:
                case Statement_Declaration:
                case Statement_Constant:

                    declare_symbol(statement);
                    break;

                case Statement_Struct:
                case Statement_Typedef:

                    declare_local_type(statement);
                    break;

                case Statement_Enum:
                    declare_symbol(statement);
                    declare_local_type(statement);

                default: break;
            }

        } else {
            // there must have been an error.
        }
    }

    list_head(parser.local_types)->length = local_types_count;
    list_head(parser.local_symbols)->length = local_symbols_count;

    parser.scope = scope->parentScope;
    return scope;
}
