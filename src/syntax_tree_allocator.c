

u32 ExpressionType_Bytesizes[] = {
    [ExprType_Plus] = sizeof(BinaryExpression),
    [ExprType_Minus] = sizeof(BinaryExpression),
    [ExprType_Mul] = sizeof(BinaryExpression),
    [ExprType_Div] = sizeof(BinaryExpression),
    [ExprType_Mod] = sizeof(BinaryExpression),
    [ExprType_Less] = sizeof(BinaryExpression),
    [ExprType_Greater] = sizeof(BinaryExpression),
    [ExprType_LessEquals] = sizeof(BinaryExpression),
    [ExprType_GreaterEquals] = sizeof(BinaryExpression),
    [ExprType_Equals] = sizeof(BinaryExpression),
    [ExprType_NotEquals] = sizeof(BinaryExpression),
    [ExprType_BooleanAnd] = sizeof(BinaryExpression),
    [ExprType_BooleanOr] = sizeof(BinaryExpression),
    [ExprType_Bitwise_And] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Or] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Xor] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Lshift] = sizeof(BinaryExpression),
    [ExprType_Bitwise_Rshift] = sizeof(BinaryExpression),
    [ExprType_Unary_PreIncrement] = sizeof(UnaryExpression),
    [ExprType_Unary_PostIncrement] = sizeof(UnaryExpression),
    [ExprType_Unary_PreDecrement] = sizeof(UnaryExpression),
    [ExprType_Unary_PostDecrement] = sizeof(UnaryExpression),
    [ExprType_Unary_Not] = sizeof(UnaryExpression),
    [ExprType_Unary_BitwiseNot] = sizeof(UnaryExpression),
    [ExprType_Unary_AddressOf] = sizeof(UnaryExpression),
    [ExprType_Unary_ValueOf] = sizeof(UnaryExpression),
    [ExprType_Unary_Negate] = sizeof(UnaryExpression),
    [ExprType_Literal_Integer] = sizeof(LiteralExpression),
    [ExprType_Literal_Decimal] = sizeof(LiteralExpression),
    [ExprType_Literal_Char] = sizeof(LiteralExpression),
    [ExprType_Literal_String] = sizeof(LiteralExpression),
    [ExprType_Literal_True] = sizeof(LiteralExpression),
    [ExprType_Literal_False] = sizeof(LiteralExpression),
    [ExprType_Literal_Null] = sizeof(LiteralExpression),
    [ExprType_Variable] = sizeof(VariableExpression),
    [ExprType_Alloc] = sizeof(AllocExpression),
    [ExprType_Ternary] = sizeof(TernaryExpression),
    [ExprType_ProcCall] = sizeof(ProcCall),
    [ExprType_Deref] = sizeof(DerefOperator),
    [ExprType_Indexing] = sizeof(IndexingExpression),
    [ExprType_Cast] = sizeof(CastExpression),
    [ExprType_Sizeof] = sizeof(SizeofExpression),
    [ExprType_Parenthesized] = sizeof(ParenthesizedExpression),
    [ExprType_Compound] = sizeof(CompoundExpression)
};

u32 StatementType_Bytesizes[] = {
    [Statement_Declaration] = sizeof(Declaration),
    [Statement_FixedArray] = sizeof(Declaration),
    [Statement_Constant] = sizeof(Declaration),
    [Statement_Typedef] = sizeof(Typedef),
    [Statement_Assignment] = sizeof(Assignment),
    [Statement_Expression] = sizeof(StatementExpression),
    [Statement_Scope] = sizeof(Scope),
    [Statement_If] = sizeof(IfStatement),
    [Statement_While] = sizeof(WhileStatement),
    [Statement_For] = sizeof(ForStatement),
    [Statement_Switch] = sizeof(SwitchStatement),
    [Statement_Procedure] = sizeof(Procedure),
    [Statement_Struct] = sizeof(Struct),
    [Statement_Enum] = sizeof(Enum),
    [Statement_Continue] = sizeof(Statement),
    [Statement_Break] = sizeof(Statement),
    [Statement_Return] = sizeof(ReturnStatement),
    [Statement_Goto] = sizeof(GotoStatement),
    [Statement_Label] = sizeof(LabelStatement),
    [Statement_CaseLabel] = sizeof(CaseLabelStatement),
    [Statement_DefaultLabel] = sizeof(Statement)
};

Node node_init() {
    Node node = {0};
    node.file_index = parser.current_file_index;
    node.lineNumber = tokens[token_index].line;
    return node;
}

void* allocate_node(u32 node_size) {
    Node* node = calloc(1, node_size);
    node->file_index = parser.current_file_index;
    node->lineNumber = tokens[token_index].line;
    return node;
}

void* allocExpr(ExprType type) {
    Expression* expr = allocate_node(ExpressionType_Bytesizes[type]);
    expr->expressionType = type;
    expr->datatype = type_invalid;
    return expr;
}

void* allocStatement(StatementType type) {
    Statement* sta = allocate_node(StatementType_Bytesizes[type]);
    sta->statementType = type;
    return sta;
}
