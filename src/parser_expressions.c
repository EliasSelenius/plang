

#define init_expr_type(type) do { p = alloc_node(parser, Node_##type); token = advance(parser); } while (0)


static NodeRef expect_leaf(Parser* parser) {
    NodeRef p = null_node;
    Token token;
    switch (peek(parser).type) {
        case Tok_Word: {
            init_expr_type(Variable);
            p.Variable->name = token.data.string;
            p.Variable->ref = get_symbol(parser, p.Variable->name);
            if (!p.Variable->ref.node) list_add(parser->unresolved_variables, p);
        } break;

        case Tok_Keyword_Alloc: {
            init_expr_type(Alloc);
            p.Alloc->type = expectType(parser);
            if (!tok(parser, Tok_OpenSquare)) break;
            p.Alloc->size_expr = expect_expr(parser);
            expect(parser, Tok_CloseSquare);
        } break;

        case Tok_Keyword_Sizeof: {
            init_expr_type(Sizeof);
            bool close = tok(parser, Tok_OpenParen);
            p.Sizeof->type = expectType(parser);
            if (close) expect(parser, Tok_CloseParen);
        } break;

        case Tok_OpenParen: {
            init_expr_type(Parenthesized);
            p.Parenthesized->inner_expr = expect_expr(parser);
            expect(parser, Tok_CloseParen);
        } break;

        case Tok_Period: {
            init_expr_type(Deref);
            p.Deref->name = identifier(parser);
        } break;

        case Tok_OpenCurl: {
            init_expr_type(Compound);
            if (tok(parser, Tok_CloseCurl)) break;

            list_init(p.Compound->elements);
            do {
                CompoundElement el = {0};
                if (peek_at(parser, 1).type == Tok_Assign) {
                    el.name = identifier(parser);
                    advance(parser);
                }

                el.expr = expect_expr(parser);
                list_add(p.Compound->elements, el);
            } while (tok(parser, Tok_Comma));

            expect(parser, Tok_CloseCurl);
        } break;

        case Tok_Integer:       init_expr_type(Literal_Integer); p.Literal->data = token.data; break;
        case Tok_Decimal:       init_expr_type(Literal_Decimal); p.Literal->data = token.data; break;
        case Tok_String:        init_expr_type(Literal_String); p.Literal->data = token.data; break;
        case Tok_Char:          init_expr_type(Literal_Char); p.Literal->data = token.data; break;
        case Tok_Keyword_True:  init_expr_type(Literal_True); break;
        case Tok_Keyword_False: init_expr_type(Literal_False); break;
        case Tok_Keyword_Null:  init_expr_type(Literal_Null); break;

        default: fatal_parse_error(parser, "Expected expression.");
    }

    again:
    NodeRef inner_node = p;
    switch (peek(parser).type) {
        case Tok_Period: {
            init_expr_type(Deref);
            p.Deref->expr = inner_node;
            p.Deref->name = identifier(parser);
        } goto again;

        case Tok_OpenSquare: {
            init_expr_type(Indexing);
            p.Indexing->indexed = inner_node;
            p.Indexing->index = expect_expr(parser);
            expect(parser, Tok_CloseSquare);
        } goto again;

        case Tok_OpenParen: {
            init_expr_type(ProcCall);
            p.ProcCall->proc_expr = inner_node;

            if (tok(parser, Tok_CloseParen)) goto again;
            list_init(p.ProcCall->args);
            do {
                NodeRef arg = expect_expr(parser);
                list_add(p.ProcCall->args, arg);
            } while (tok(parser, Tok_Comma));
            expect(parser, Tok_CloseParen);
        } goto again;

        default: break;
    }

    return p;
}

static NodeRef expect_unary(Parser* parser) {
    NodeRef p = null_node;
    Token token;
    switch (peek(parser).type) {
        case Tok_Mul:             init_expr_type(Unary_AddressOf); break;
        case Tok_At:              init_expr_type(Unary_ValueOf); break; // TODO: it would be nice to have syntax like @@my_double_ptr
        case Tok_ExclamationMark: init_expr_type(Unary_Not); break;
        case Tok_PlusPlus:        init_expr_type(Unary_PreIncrement); break;
        case Tok_MinusMinus:      init_expr_type(Unary_PreDecrement); break;
        case Tok_Minus:           init_expr_type(Unary_Negate); break;
        case Tok_Tilde:           init_expr_type(Unary_BitwiseNot); break;
        default: break;
    }

    NodeRef inner = expect_leaf(parser);
    if (p.node) {
        p.Unary->inner_expr = inner;
        inner = p;
    }

    switch (peek(parser).type) {
        case Tok_PlusPlus:   init_expr_type(Unary_PostIncrement); p.Unary->inner_expr = inner; inner = p; break;
        case Tok_MinusMinus: init_expr_type(Unary_PostDecrement); p.Unary->inner_expr = inner; inner = p; break;
        default: break;
    }

    if (peek(parser).type == Tok_Keyword_As) {
        init_expr_type(Cast);
        p.Cast->expr = inner;
        p.Cast->new_type = expectType(parser);
    }

    return p.node ? p : inner;
}

#undef init_expr_type



static u32 binary_precedence_level(NodeRef a) {
    switch (a.node->kind) {
        case Node_BooleanAnd:
        case Node_BooleanOr:
            return 1;

        case Node_Less:
        case Node_Greater:
        case Node_LessEquals:
        case Node_GreaterEquals:
        case Node_Equals:
        case Node_NotEquals:
            return 2;

        case Node_Bitwise_Lshift:
        case Node_Bitwise_Rshift:
        case Node_Bitwise_And:
        case Node_Bitwise_Or:
        case Node_Bitwise_Xor:
            return 3;

        case Node_Plus:
        case Node_Minus:
            return 4;
        case Node_Mul:
        case Node_Div:
        case Node_Mod:
            return 5;

        default: return 0; // not an operator
    }
}

static bool is_binary_expr(NodeRef a) { return binary_precedence_level(a) != 0; }

static Nodekind token_is_binary(TokenType type) {
    switch (type) {
        case Tok_Plus: return Node_Plus;
        case Tok_Minus: return Node_Minus;
        case Tok_Mul: return Node_Mul;
        case Tok_Div: return Node_Div;
        case Tok_Mod: return Node_Mod;

        case Tok_LessThan: return Node_Less;
        case Tok_GreaterThan: return Node_Greater;
        case Tok_LessThanOrEqual: return Node_LessEquals;
        case Tok_GreaterThanOrEqual: return Node_GreaterEquals;
        case Tok_Equals: return Node_Equals;
        case Tok_NotEquals: return Node_NotEquals;
        case Tok_Keyword_And: return Node_BooleanAnd;
        case Tok_Keyword_Or: return Node_BooleanOr;

        case Tok_Ampersand: return Node_Bitwise_And;
        case Tok_Pipe: return Node_Bitwise_Or;
        case Tok_Caret: return Node_Bitwise_Xor;
        case Tok_LeftShift: return Node_Bitwise_Lshift;
        case Tok_RightShift: return Node_Bitwise_Rshift;

        default: return 0; // not an operator
    }
}

/*
1 + 2 * 3

    +
   1 *
    2 3

a = (1 + 2)
b = ( * 3)

merge(a, b)
    2 => merge(2, b)
    
*/


static NodeRef merge_binary_exprs(NodeRef a, NodeRef b) {
    if (binary_precedence_level(a) >= binary_precedence_level(b)) {
        b.Binary->left = a;
        return b;
    }

    if (is_binary_expr(a.Binary->right)) {
        a.Binary->right = merge_binary_exprs(a.Binary->right, b);
        return a;
    }

    b.Binary->left = a.Binary->right;
    a.Binary->right = b;
    return a;
}

static NodeRef expect_binary(Parser* parser) {
    NodeRef a = expect_unary(parser);
    Nodekind node_kind = token_is_binary(peek(parser).type);
    if (node_kind == 0) return a;

    NodeRef root = alloc_node(parser, node_kind);
    advance(parser);
    root.Binary->left = a;
    root.Binary->right = expect_unary(parser);

    while ((node_kind = token_is_binary(peek(parser).type))) {
        NodeRef op = alloc_node(parser, node_kind);
        advance(parser);
        op.Binary->right = expect_unary(parser);
        root = merge_binary_exprs(root, op);
    }

    return root;
}

static NodeRef expect_expr(Parser* parser) {
    NodeRef res = expect_binary(parser);

    if (!tok(parser, Tok_QuestionMark)) return res;
    NodeRef p = alloc_node(parser, Node_Ternary);
    p.Ternary->condition = res;
    p.Ternary->then_expr = expect_expr(parser);
    expect(parser, Tok_Colon);
    p.Ternary->else_expr = expect_expr(parser);
    return p;
}
