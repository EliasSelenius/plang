

static u32 numberOfErrors = 0;

static void error_v(u32 line, char* filename, char* format, va_list args) {
    printf("%s:%d: error: ", filename, line);
    vprintf(format, args);
    printf("\n");
    numberOfErrors++;
}

static void error_temp(char* format, ...) {
    va_list args;
    va_start(args, format);
    error_v(0, "", format, args);
    va_end(args);
}

static void error(u32 line, char* filename, char* format, ...) {
    va_list args;
    va_start(args, format);
    error_v(line, filename, format, args);
    va_end(args);
}

static void error_at_token(u32 tokenIndex, char* format, ...) {
    Token* tok = &tokens[tokenIndex];
    va_list args;
    va_start(args, format);
    error_v(tok->line, g_Filename, format, args);
    va_end(args);
}

static void error_token(char* format, ...) {
    Token* tok = &tokens[token_index];
    va_list args;
    va_start(args, format);
    error_v(tok->line, g_Filename, format, args);
    va_end(args);
}

static void error_node(void* node, char* format, ...) {
    Node* n = (Node*)node;

    va_list args;
    va_start(args, format);
    // error_v(n->lineNumber, n->filepath, format, args);
    error_v(0, "", format, args); // TODO: ...
    va_end(args);
}