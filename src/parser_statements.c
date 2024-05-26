

static NodeRef optional_sub_node(Parser* parser) { return tok(parser, Tok_Semicolon) ? null_node : expect_node(parser); }

static NodeRef expect_node(Parser* parser) {
    switch (peek(parser).type) {

        case Tok_OpenCurl: return (NodeRef)expectScope(parser);

        case Tok_Keyword_While: {
            WhileStmt* whileStatement = alloc_node(parser, Node_WhileStmt).WhileStmt;
            advance(parser);

            whileStatement->condition = expect_expr(parser);
            whileStatement->statement = optional_sub_node(parser);

            return (NodeRef)whileStatement;
        }

        case Tok_Keyword_If: {
            IfStmt* res = alloc_node(parser, Node_IfStmt).IfStmt;
            advance(parser);

            res->condition = expect_expr(parser);
            res->then_statement = optional_sub_node(parser);
            if (tok(parser, Tok_Keyword_Else)) res->else_statement = expect_node(parser);

            return (NodeRef)res;
        }

        case Tok_Keyword_For: {
            ForStmt* forsta = alloc_node(parser, Node_ForStmt).ForStmt;
            advance(parser);

            bool must_close = tok(parser, Tok_OpenParen);

            TokenType expected_token = Tok_Comma;
            TokenType tt = parser->tokens[peekType(parser) + 1].type;

            if (peek_at(parser, 1).type == Tok_Colon) { // for i :
                forsta->index_name = identifier(parser);
                advance(parser);
                expected_token = Tok_Dotdot;
            } else if (tt == Tok_Colon) { // for uint32 i :
                forsta->index_type = expectType(parser);
                forsta->index_name = identifier(parser);
                advance(parser); // advance over ':'
                expected_token = Tok_Dotdot;
            } else if (tt == Tok_Assign) { // for uint32 i =
                forsta->index_type = expectType(parser);
                forsta->index_name = identifier(parser);
                advance(parser); // advance over '='
                forsta->iterator_assignment = expect_expr(parser);
                expect(parser, Tok_Comma);
            } else if (tt == Tok_Comma) { // for uint32 i,
                forsta->index_type = expectType(parser);
                forsta->index_name = identifier(parser);
                advance(parser); // advance over ','
            } else { // for min_expr (.. max_expr)?
                forsta->index_name = builtin_string_it;
                expected_token = Tok_Dotdot;
            }

            declare_symbol(parser, (NodeRef)forsta);

            forsta->condition = expect_expr(parser);
            if (tok(parser, expected_token)) forsta->iterator_update = expect_expr(parser);

            if (must_close) expect(parser, Tok_CloseParen);

            forsta->statement = optional_sub_node(parser);
            stack_pop(parser, 1);
            return (NodeRef)forsta;
        }

        {
            static SwitchStmt* current_switch = null;

            case Tok_Keyword_Switch: {
                SwitchStmt* switchStatement = alloc_node(parser, Node_SwitchStmt).SwitchStmt;
                advance(parser);

                switchStatement->expr = expect_expr(parser);

                SwitchStmt* temp = current_switch;
                current_switch = switchStatement;
                switchStatement->scope = expectScope(parser);
                current_switch = temp;

                return (NodeRef)switchStatement;
            }

            case Tok_Keyword_Case: {
                CaseLabelStmt* caseLabel = alloc_node(parser, Node_CaseLabelStmt).CaseLabelStmt;
                advance(parser);
                caseLabel->expr = expect_expr(parser);
                caseLabel->switch_statement = current_switch;
                expect(parser, Tok_Colon);
                return (NodeRef)caseLabel;
            }
        }


        case Tok_Keyword_Default: {
            DefaultLabelStmt* sta = alloc_node(parser, Node_DefaultLabelStmt).DefaultLabelStmt;
            advance(parser);
            expect(parser, Tok_Colon);
            return (NodeRef)sta;
        }

        case Tok_Keyword_Continue: {
            ContinueStmt* sta = alloc_node(parser, Node_ContinueStmt).ContinueStmt;
            advance(parser);
            semicolon(parser);
            return (NodeRef)sta;
        }

        case Tok_Keyword_Break: {
            BreakStmt* sta = alloc_node(parser, Node_BreakStmt).BreakStmt;
            advance(parser);
            semicolon(parser);
            return (NodeRef)sta;
        }

        case Tok_Keyword_Return: {
            ReturnStmt* ret = alloc_node(parser, Node_ReturnStmt).ReturnStmt;
            advance(parser);
            if (peek(parser).type != Tok_Semicolon) ret->expr = expect_expr(parser);
            semicolon(parser);
            return (NodeRef)ret;
        }

        case Tok_Keyword_Goto: {
            GotoStmt* go = alloc_node(parser, Node_GotoStmt).GotoStmt;
            advance(parser);
            go->label = identifier(parser);
            semicolon(parser);
            return (NodeRef)go;
        }

        case Tok_Keyword_Struct: return expectStruct(parser);
        case Tok_Keyword_Enum: return expectEnum(parser);
        case Tok_Keyword_Type: return expectTypedef(parser);
        case Tok_Keyword_Const: return expectConst(parser);

        case Tok_Keyword_Static: {
            advance(parser);
            Declaration* decl = alloc_node(parser, Node_Declaration).Declaration;
            decl->type = expectType(parser);
            decl->name = identifier(parser);
            decl->is_static = true;
            if (tok(parser, Tok_Assign)) decl->expr = expect_expr(parser);
            semicolon(parser);
            return (NodeRef)decl;
        }

        case Tok_Keyword_Include: {
            advance(parser);
            Identifier file_name = expect(parser, Tok_String).data.string;
            printf("include \"%s\"\n", get_string(file_name));
            list_add(parser->current_unit->included_files, file_name);
            semicolon(parser);
            return null_node;
        }

        case Tok_EOF: return null_node;

        default: break;
    }

    // label
    if (peek(parser).type == Tok_Word && peek_at(parser, 1).type == Tok_Colon) {
        LabelStmt* label = alloc_node(parser, Node_LabelStmt).LabelStmt;
        label->label = advance(parser).data.string;
        advance(parser);
        return (NodeRef)label;
    }

    // declaration or procedure
    u32 postType = peekType(parser);
    if (postType && parser->tokens[postType].type == Tok_Word) {
        return proc_or_var(parser, true);
    }


    NodeRef expr = expect_expr(parser);

    Token* token = anyof(parser, 8, Tok_Assign,
                                    Tok_PlusAssign,
                                    Tok_MinusAssign,
                                    Tok_MulAssign,
                                    Tok_DivAssign,
                                    Tok_BitAndAssign,
                                    Tok_BitOrAssign,
                                    Tok_BitXorAssign);

    if (token) {
        Assignment* ass = alloc_node(parser, Node_Assignment).Assignment;
        ass->dst_expr = expr;
        ass->operator = token->type;
        ass->src_expr = expect_expr(parser);
        semicolon(parser);
        return (NodeRef)ass;
    }

    semicolon(parser);

    switch (expr.node->kind) {
        case Node_Unary_PreIncrement: break;
        case Node_Unary_PostIncrement: break;
        case Node_Unary_PreDecrement: break;
        case Node_Unary_PostDecrement: break;
        case Node_ProcCall: break;

        default: if (!parser->allow_lonely_expressions) error_token(parser, "This expression is all by its lonesome.");
    }

    return expr;
}

static Scope* expectScope(Parser* parser) {
    Scope* scope = alloc_node(parser, Node_Scope).Scope;
    list_init(scope->statements);

    scope->parentScope = parser->scope;
    parser->scope = scope;

    u32 local_types_count = list_length(parser->local_types);
    u32 local_symbols_count = list_length(parser->local_symbols);

    expect(parser, Tok_OpenCurl);
    while (!tok(parser, Tok_CloseCurl)) {
        NodeRef ref = expect_node(parser);
        if (!ref.node) continue; // there must have been an error.

        list_add(scope->statements, ref);

        switch (ref.node->kind) {
            // case Node_ForStmt: // declare_symbol() is called in expect_node(parser) for for-loops, because it must be called before we parse inner statement
            // case Node_Procedure: // declare_symbol() is called in proc_or_var() for same reason as above

            case Node_Declaration:
            case Node_Constant:
                declare_symbol(parser, ref);
                break;

            case Node_Struct:
            case Node_Typedef:
                declare_local_type(parser, ref);
                break;

            case Node_Enum:
                declare_symbol(parser, ref);
                declare_local_type(parser, ref);

            default: break;
        }
    }

    list_head(parser->local_types)->length = local_types_count;
    list_head(parser->local_symbols)->length = local_symbols_count;

    parser->scope = scope->parentScope;
    return scope;
}
