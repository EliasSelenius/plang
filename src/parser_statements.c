
static bool isBasicType(Parser* parser);
static bool _isBasicType_validModifier(Parser* parser) {

    u32 ti = parser->token_index;

    if (tok(parser, Tok_Mul)) return true;

    if (tok(parser, Tok_OpenParen)) {
        if (tok(parser, Tok_CloseParen)) return true;

        do {
            if (!isBasicType(parser)) goto nope;
        } while (tok(parser, Tok_Comma));

        if (tok(parser, Tok_CloseParen)) return true;
        else goto nope;
    }

    if (tok(parser, Tok_OpenSquare)) {
        if (tok(parser, Tok_CloseSquare)) return true;
        if (tok(parser, Tok_Dotdot) && tok(parser, Tok_CloseSquare)) return true;
        if (tok(parser, Tok_Integer) && tok(parser, Tok_CloseSquare)) return true;
    }

    nope:
    parser->token_index = ti;
    return false;
}

static bool isBasicType(Parser* parser) {
    if (tok(parser, Tok_Word)) {
        if (tok(parser, Tok_Period)) if (!tok(parser, Tok_Word)) return false;
        while (_isBasicType_validModifier(parser));
        return true;
    }
    return false;
}

static u32 peekType(Parser* parser) {

    if (peek(parser).type == Tok_Keyword_Let) return parser->token_index + 1;

    u32 ti = parser->token_index;
    u32 res = 0;
    if (isBasicType(parser)) res = parser->token_index;
    parser->token_index = ti;
    return res;
}


static Statement* expectStatement(Parser* parser) {
    switch (peek(parser).type) {

        case Tok_OpenCurl: return (Statement*)expectScope(parser);

        case Tok_Keyword_While: {
            WhileStatement* whileStatement = allocStatement(parser, Statement_While);
            advance(parser);

            whileStatement->condition = expectExpression(parser);
            whileStatement->statement = tok(parser, Tok_Semicolon) ? null : expectStatement(parser);

            return (Statement*)whileStatement;
        }

        case Tok_Keyword_If: {
            IfStatement* res = allocStatement(parser, Statement_If);
            advance(parser);

            res->condition = expectExpression(parser);
            res->then_statement = tok(parser, Tok_Semicolon) ? null : expectStatement(parser);
            if (tok(parser, Tok_Keyword_Else)) res->else_statement = expectStatement(parser);

            return (Statement*)res;
        }

        case Tok_Keyword_For: {
            ForStatement* forsta = allocStatement(parser, Statement_For);
            advance(parser);

            if (peek_at(parser, 1).type == Tok_Colon) {
                forsta->index_name = identifier(parser);
                advance(parser);
            } else if (parser->tokens[peekType(parser) + 1].type == Tok_Colon) {
                forsta->index_type = expectType(parser);
                forsta->index_name = identifier(parser);
                advance(parser);
            } else {
                forsta->index_name = builtin_string_it;
            }

            forsta->min_expr = expectExpression(parser);

            if (tok(parser, Tok_Dotdot)) {
                forsta->max_expr = expectExpression(parser);
            }

            declare_symbol(parser, (Statement*)forsta);
            forsta->statement = expectStatement(parser);
            stack_pop(parser, 1);
            return (Statement*)forsta;
        }

        {
            static SwitchStatement* current_switch = null;

            case Tok_Keyword_Switch: {
                SwitchStatement* switchStatement = allocStatement(parser, Statement_Switch);
                advance(parser);

                switchStatement->expr = expectExpression(parser);

                SwitchStatement* temp = current_switch;
                current_switch = switchStatement;
                switchStatement->scope = expectScope(parser);
                current_switch = temp;

                return (Statement*)switchStatement;
            }

            case Tok_Keyword_Case: {
                CaseLabelStatement* caseLabel = allocStatement(parser, Statement_CaseLabel);
                advance(parser);
                caseLabel->expr = expectExpression(parser);
                caseLabel->switch_statement = current_switch;
                expect(parser, Tok_Colon);
                return (Statement*)caseLabel;
            }
        }


        case Tok_Keyword_Default: {
            Statement* sta = allocStatement(parser, Statement_DefaultLabel);
            advance(parser);
            expect(parser, Tok_Colon);
            return sta;
        }

        case Tok_Keyword_Continue: {
            Statement* sta = allocStatement(parser, Statement_Continue);
            advance(parser);
            semicolon(parser);
            return sta;
        }

        case Tok_Keyword_Break: {
            Statement* sta = allocStatement(parser, Statement_Break);
            advance(parser);
            semicolon(parser);
            return sta;
        }

        case Tok_Keyword_Return: {
            ReturnStatement* ret = allocStatement(parser, Statement_Return);
            advance(parser);
            ret->returnExpr = parseExpression(parser);
            semicolon(parser);
            return (Statement*)ret;
        }

        case Tok_Keyword_Goto: {
            GotoStatement* go = allocStatement(parser, Statement_Goto);
            advance(parser);
            go->label = identifier(parser);
            semicolon(parser);
            return (Statement*)go;
        }

        case Tok_Keyword_Struct: return expectStruct(parser);
        case Tok_Keyword_Enum: return expectEnum(parser);
        case Tok_Keyword_Type: return expectTypedef(parser);
        case Tok_Keyword_Const: return expectConst(parser);

        case Tok_Keyword_Static: {
            advance(parser);
            Declaration* decl = allocStatement(parser, Statement_Declaration);
            decl->type = expectType(parser);
            decl->name = identifier(parser);
            decl->is_static = true;
            if (tok(parser, Tok_Assign)) decl->expr = expectExpression(parser);
            semicolon(parser);
            return (Statement*)decl;
        }

        default: break;
    }

    // label
    if (peek(parser).type == Tok_Word && peek_at(parser, 1).type == Tok_Colon) {
        LabelStatement* label = allocStatement(parser, Statement_Label);
        label->label = advance(parser).data.string;
        advance(parser);
        return (Statement*)label;
    }

    // declaration or procedure
    u32 postType = peekType(parser);
    if (postType && parser->tokens[postType].type == Tok_Word) {
        return proc_or_var(parser, true);
    }


    // expression statement
    Expression* expr = parseExpression(parser);
    if (!expr) {
        fatal_parse_error(parser, "Unexpected token.");
        return null;
    }

    Token* token = anyof(parser, 8, Tok_Assign,
                                    Tok_PlusAssign,
                                    Tok_MinusAssign,
                                    Tok_MulAssign,
                                    Tok_DivAssign,
                                    Tok_BitAndAssign,
                                    Tok_BitOrAssign,
                                    Tok_BitXorAssign);

    if (token) {
        Assignment* ass = allocStatement(parser, Statement_Assignment);
        ass->base.nodebase.lineNumber = expr->nodebase.lineNumber;
        ass->assigneeExpr = expr;
        ass->assignmentOper = token->type;
        ass->expr = expectExpression(parser);
        semicolon(parser);
        return (Statement*)ass;
    }


    StatementExpression* staExpr = allocStatement(parser, Statement_Expression);
    staExpr->base.nodebase.lineNumber = expr->nodebase.lineNumber;
    staExpr->expr = expr;
    semicolon(parser);
    return (Statement*)staExpr;

    // switch (expr->expressionType) {
    //     case ExprType_Unary_PreIncrement:
    //     case ExprType_Unary_PostIncrement:
    //     case ExprType_Unary_PreDecrement:
    //     case ExprType_Unary_PostDecrement:
    //     case ExprType_ProcCall: {
    //         StatementExpression* staExpr = allocStatement(parser, Statement_Expression);
    //         staExpr->base.nodebase.lineNumber = expr->nodebase.lineNumber;
    //         staExpr->expr = expr;
    //         semicolon(parser);
    //         return (Statement*)staExpr;
    //     }

    //     case ExprType_Indexing:
    //     case ExprType_Unary_ValueOf:
    //     case ExprType_Variable:
    //     case ExprType_Deref: {

    //         Token* token = anyof(8, Tok_Assign,
    //                                 Tok_PlusAssign,
    //                                 Tok_MinusAssign,
    //                                 Tok_MulAssign,
    //                                 Tok_DivAssign,
    //                                 Tok_BitAndAssign,
    //                                 Tok_BitOrAssign,
    //                                 Tok_BitXorAssign);
    //         if (token) {
    //             Assignment* ass = allocStatement(parser, Statement_Assignment);
    //             ass->base.nodebase.lineNumber = expr->nodebase.lineNumber;
    //             ass->assigneeExpr = expr;
    //             ass->assignmentOper = token->type;
    //             ass->expr = expectExpression(parser);
    //             semicolon(parser);
    //             return (Statement*)ass;
    //         }

    //         error_token(parser, "Expected an assignment.");
    //     } break;


    //     default:
    //         error_token(parser, "This expression is all by its lonesome.");
    //         return null;
    // }
    // return null;
}


static Scope* expectScope(Parser* parser) {
    Scope* scope = allocStatement(parser, Statement_Scope);
    scope->statements = list_create(Statement*);

    scope->parentScope = parser->scope;
    parser->scope = scope;

    u32 local_types_count = list_length(parser->local_types);
    u32 local_symbols_count = list_length(parser->local_symbols);

    expect(parser, Tok_OpenCurl);
    while (!tok(parser, Tok_CloseCurl)) {
        Statement* statement = expectStatement(parser);
        if (statement) {
            list_add(scope->statements, statement);

            switch (statement->statementType) {
                // case Statement_For: // declare_symbol() is called in expectStatement(parser) for for-loops, because it must be called before we parse inner statement
                // case Statement_Procedure: // declare_symbol() is called in proc_or_var() for same reason as above

                case Statement_Declaration:
                case Statement_Constant:

                    declare_symbol(parser, statement);
                    break;

                case Statement_Struct:
                case Statement_Typedef:

                    declare_local_type(parser, statement);
                    break;

                case Statement_Enum:
                    declare_symbol(parser, statement);
                    declare_local_type(parser, statement);

                default: break;
            }

        } else {
            // there must have been an error.
        }
    }

    list_head(parser->local_types)->length = local_types_count;
    list_head(parser->local_symbols)->length = local_symbols_count;

    parser->scope = scope->parentScope;
    return scope;
}
