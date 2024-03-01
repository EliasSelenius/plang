


static u32 current_line;
static u32 current_column;
static char* cursor;

static char* advance_next_char() {
    current_column++;
    return cursor++;
}

static void advance_next_line() {
    current_line++;
    current_column = 0;
}

static Token construct_token_here(TokenType type) {
    Token t = {0};
    t.type = type;
    t.line = current_line;
    t.column = current_column;
    return t;
}

static inline void appendToken(Parser* parser, TokenType type) {
    Token token = construct_token_here(type);
    list_add(parser->tokens, token);
}

static void lexer_error(Parser* parser, const char* format, ...) {
    CodeLocation loc = { .file_name = parser->current_file_name, .line = current_line, .column = current_column };

    va_list args;
    va_start(args, format);
    gen_errorv(parser, loc, format, args);
    va_end(args);
}

static u64 hex_char_to_value(char h) {
    if (h >= '0' && h <= '9') return h - '0';
    if (h >= 'a' && h <= 'f') return 10 + h - 'a';
    if (h >= 'A' && h <= 'F') return 10 + h - 'A';

    return -1;
}

static u64 hex_number(u32* numDigits) {
    char* start = cursor;
    while (isHexDigit(*cursor) || *cursor == '_') advance_next_char();
    u32 len = cursor - start;

    u32 num_underscores = 0;

    u64 acc = 0;
    u64 place = 1;
    for (i32 i = len - 1; i >= 0; i--) {
        if (start[i] == '_') { num_underscores++; continue; }
        u64 d = hex_char_to_value(start[i]);
        acc += place * d;
        place *= 16;
    }

    *numDigits = len - num_underscores;
    return acc;
}

static u64 number(u32* numDigits) {
    char* start = cursor;
    while (isDigit(*++cursor) || *cursor == '_');
    u32 len = cursor - start;

    u32 num_underscores = 0;

    u64 acc = 0;
    u64 place = 1;
    for (i32 i = len - 1; i >= 0; i--) {

        if (start[i] == '_') { num_underscores++; continue; }

        u64 d = start[i] - '0';
        acc += place * d;
        place *= 10;
    }

    *numDigits = len - num_underscores;
    return acc;
}



static void lex(Parser* parser, char* input) {

    list_clear(parser->tokens);

    cursor = input - 1;
    current_line = 1;
    current_column = 0;

    while (true) {
        advance_next_char();



        // TODO: this should be a switch statement as it would be faster
        //       ĉi tio devus esti ŝaltilo deklaro ĉar ĝi estus pli rapida

        if (*cursor == '\0') {
            appendToken(parser, Tok_EOF);
            break;
        }


        { // handle whitespace
            if (*cursor == ' ') continue;

            if (*cursor == '\n') {
                advance_next_line();
                continue;
            }
        }

        // Word
        if (isLetter(*cursor) || (*cursor == '_')) {
            char* wordStart = cursor;
            while (isLetter(*++cursor) || isDigit(*cursor) || (*cursor == '_'));
            cursor--;

            StrSpan word = {
                .start = wordStart,
                .length = cursor - (wordStart - 1)
            };

            TokenType tokType = Tok_Word;

            // Keywords
            if (spanEquals(word, "struct")) tokType = Tok_Keyword_Struct;
            else if (spanEquals(word, "enum")) tokType = Tok_Keyword_Enum;
            else if (spanEquals(word, "alloc")) tokType = Tok_Keyword_Alloc;
            else if (spanEquals(word, "let")) tokType = Tok_Keyword_Let;
            else if (spanEquals(word, "include")) tokType = Tok_Keyword_Include;
            else if (spanEquals(word, "if")) tokType = Tok_Keyword_If;
            else if (spanEquals(word, "else")) tokType = Tok_Keyword_Else;
            else if (spanEquals(word, "while")) tokType = Tok_Keyword_While;
            else if (spanEquals(word, "true")) tokType = Tok_Keyword_True;
            else if (spanEquals(word, "false")) tokType = Tok_Keyword_False;
            else if (spanEquals(word, "and")) tokType = Tok_Keyword_And;
            else if (spanEquals(word, "or")) tokType = Tok_Keyword_Or;
            else if (spanEquals(word, "null")) tokType = Tok_Keyword_Null;
            else if (spanEquals(word, "continue")) tokType = Tok_Keyword_Continue;
            else if (spanEquals(word, "break")) tokType = Tok_Keyword_Break;
            else if (spanEquals(word, "return")) tokType = Tok_Keyword_Return;
            else if (spanEquals(word, "type")) tokType = Tok_Keyword_Type;
            else if (spanEquals(word, "as")) tokType = Tok_Keyword_As;
            else if (spanEquals(word, "const")) tokType = Tok_Keyword_Const;
            else if (spanEquals(word, "for")) tokType = Tok_Keyword_For;
            else if (spanEquals(word, "switch")) tokType = Tok_Keyword_Switch;
            else if (spanEquals(word, "default")) tokType = Tok_Keyword_Default;
            else if (spanEquals(word, "case")) tokType = Tok_Keyword_Case;
            else if (spanEquals(word, "goto")) tokType = Tok_Keyword_Goto;
            else if (spanEquals(word, "with")) tokType = Tok_Keyword_With;
            else if (spanEquals(word, "namespace")) tokType = Tok_Keyword_Namespace;
            else if (spanEquals(word, "sizeof")) tokType = Tok_Keyword_Sizeof;
            else if (spanEquals(word, "static")) tokType = Tok_Keyword_Static;


            Token token = construct_token_here(tokType);
            if (token.type == Tok_Word) {
                token.data.string = register_string(word);
            }

            list_add(parser->tokens, token);

            continue;
        }

        // hex number
        if (*cursor == '0' && *(cursor + 1) == 'x') {
            cursor += 2;

            Token token = construct_token_here(Tok_Integer);

            u32 numDigits;
            token.data.integer = hex_number(&numDigits);
            cursor--;

            list_add(parser->tokens, token);
            continue;
        }

        // number
        if (isDigit(*cursor)) {
            Token token = construct_token_here(Tok_Integer);

            u32 numDigits;
            u64 num = number(&numDigits);
            token.data.integer = num;

            if (*cursor == '.' && isDigit(*(cursor+1))) {
                advance_next_char();
                f64 fraction = (f64)number(&numDigits);

                f64 denom = 1.0;
                for (u32 i = 0; i < numDigits; i++) denom *= 10.0;

                f64 value = (f64)num + (fraction / denom);

                token.type = Tok_Decimal;
                token.data.decimal = value;
            }

            switch (*cursor) {
                case 'f': break;
                case 'd': break;
                case 'u': {
                    if (*(cursor + 1) == 'l') advance_next_char();
                } break;
                case 'l': break;
                default: cursor--; break;
            }

            list_add(parser->tokens, token);
            continue;
        }

        // chars
        if (*cursor == '\'') {
            char* strStart = advance_next_char();

            char c = *cursor;
            if (c == '\\') {
                advance_next_char();

                switch (*cursor) {
                    case 'n': c = '\n'; break;
                    case 't': c = '\t'; break;
                    case '\\': c = '\\'; break;
                    case '\'': c = '\''; break;
                }

            } else {
                if (c > '~' || c < ' ') lexer_error(parser, "Invalid character.");
            }

            advance_next_char();
            if (*cursor != '\'') lexer_error(parser, "Missing closing single quote.");

            Token token = construct_token_here(Tok_Char);
            token.data.character = c;
            list_add(parser->tokens, token);
            continue;
        }

        // string
        if (*cursor == '"') {
            char* strStart = cursor + 1;
            while ( !(*++cursor == '"' && *(cursor - 1) != '\\') ) {
                if (*cursor == '\n') {
                    // TODO: consider whether we want to allow multi-line strings
                    lexer_error(parser, "Strings cannot contain new-lines.");
                    advance_next_line();
                }
            }

            StrSpan str = {
                .start = strStart,
                .length = cursor - strStart
            };

            Token token = construct_token_here(Tok_String);
            token.data.string = register_string(str);
            list_add(parser->tokens, token);
            continue;
        }

        // Line comment
        if (*cursor == '/' && *(cursor + 1) == '/') {
            char* commentStart = cursor;
            while (!(*++cursor == '\n' || *cursor == '\0'));
            cursor--;
            continue;
        }

        // Multi-line comment
        if (*cursor == '/' && *(cursor + 1) == '*') {
            char* commentStart = cursor;
            cursor += 1;
            while ( !(*++cursor == '*' && *(cursor + 1) == '/') ) {
                if (*cursor == '\n') advance_next_line();
            }
            advance_next_char();
            continue;
        }


        switch (*cursor) {

            case ',': appendToken(parser, Tok_Comma); continue;

            case '.': switch (*(cursor + 1)) {
                case '.': appendToken(parser, Tok_Dotdot); advance_next_char(); continue;
                default: appendToken(parser, Tok_Period); continue;
            }

            case ';': appendToken(parser, Tok_Semicolon); continue;
            case ':': appendToken(parser, Tok_Colon); continue;
            case '?': appendToken(parser, Tok_QuestionMark); continue;
            case '@': appendToken(parser, Tok_At); continue;
            case '~': appendToken(parser, Tok_Tilde); continue;

            case '&': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_BitAndAssign); advance_next_char(); continue;
                default: appendToken(parser, Tok_Ampersand); continue;
            }
            case '|': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_BitOrAssign); advance_next_char(); continue;
                default: appendToken(parser, Tok_Pipe); continue;
            }
            case '^': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_BitXorAssign); advance_next_char(); continue;
                default: appendToken(parser, Tok_Caret); continue;
            }

            case '{': appendToken(parser, Tok_OpenCurl); continue;
            case '}': appendToken(parser, Tok_CloseCurl); continue;
            case '(': appendToken(parser, Tok_OpenParen); continue;
            case ')': appendToken(parser, Tok_CloseParen); continue;
            case '[': appendToken(parser, Tok_OpenSquare); continue;
            case ']': appendToken(parser, Tok_CloseSquare); continue;

            case '<': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_LessThanOrEqual); advance_next_char(); continue;
                case '<': appendToken(parser, Tok_LeftShift); advance_next_char(); continue;
                default: appendToken(parser, Tok_LessThan); continue;
            }

            case '>': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_GreaterThanOrEqual); advance_next_char(); continue;
                case '>': appendToken(parser, Tok_RightShift); advance_next_char(); continue;
                default: appendToken(parser, Tok_GreaterThan); continue;
            }

            case '+': switch (*(cursor + 1)) {
                case '+': appendToken(parser, Tok_PlusPlus); advance_next_char(); continue;
                case '=': appendToken(parser, Tok_PlusAssign); advance_next_char(); continue;
                default: appendToken(parser, Tok_Plus); continue;
            }

            case '-': switch (*(cursor + 1)) {
                case '-': appendToken(parser, Tok_MinusMinus); advance_next_char(); continue;
                case '=': appendToken(parser, Tok_MinusAssign); advance_next_char(); continue;
                default: appendToken(parser, Tok_Minus); continue;
            }

        }

        #define test_op_eq(c, token, tokenEqual) \
            if (*cursor == c) { \
                if (*(cursor + 1) == '=') { \
                    appendToken(parser, tokenEqual); \
                    advance_next_char(); \
                } else appendToken(parser, token); \
                continue; \
            } \


        test_op_eq('=', Tok_Assign, Tok_Equals);
        test_op_eq('!', Tok_ExclamationMark, Tok_NotEquals);
        test_op_eq('*', Tok_Mul, Tok_MulAssign);
        test_op_eq('/', Tok_Div, Tok_DivAssign);
        test_op_eq('%', Tok_Mod, Tok_ModAssign);

        #undef test_op_eq

        // Error: token not recognized
        lexer_error(parser, "Unknown byte value encountered %d.", *cursor);
    }
}
