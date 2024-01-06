


static u32 current_line;
static char* cursor;

static inline void appendToken(Parser* parser, TokenType type) {
    Token token;
    token.type = type;
    token.line = current_line;
    list_add(parser->tokens, token);
}

static u64 hex_char_to_value(char h) {
    if (h >= '0' && h <= '9') return h - '0';
    if (h >= 'a' && h <= 'f') return 10 + h - 'a';
    if (h >= 'A' && h <= 'F') return 10 + h - 'A';

    return -1;
}

static u64 hex_number(u32* numDigits) {
    char* start = cursor;
    while (isHexDigit(*cursor) || *cursor == '_') cursor++;
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

    while (true) {
        cursor++;



        // TODO: this should be a switch statement as it would be faster
        //       ĉi tio devus esti ŝaltilo deklaro ĉar ĝi estus pli rapida

        if (*cursor == '\0') {
            appendToken(parser, Tok_EOF);
            break;
        }


        { // handle whitespace
            if (*cursor == ' ') continue;

            if (*cursor == '\n') {
                current_line++;
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


            Token token;
            token.type = tokType;
            token.line = current_line;

            if (token.type == Tok_Word) {
                token.data.string = register_string(word);
            }

            list_add(parser->tokens, token);

            continue;
        }

        // hex number
        if (*cursor == '0' && *(cursor + 1) == 'x') {
            cursor += 2;

            Token token = {0};
            token.line = current_line;
            token.type = Tok_Integer;

            u32 numDigits;
            token.data.integer = hex_number(&numDigits);
            cursor--;

            list_add(parser->tokens, token);
            continue;
        }

        // number
        if (isDigit(*cursor)) {
            Token token = {0};
            token.line = current_line;
            token.type = Tok_Integer;

            u32 numDigits;
            u64 num = number(&numDigits);
            token.data.integer = num;

            if (*cursor == '.' && isDigit(*(cursor+1))) {
                cursor++;
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
                    if (*(cursor + 1) == 'l') cursor++;
                } break;
                case 'l': break;
                default: cursor--; break;
            }

            list_add(parser->tokens, token);
            continue;
        }

        // chars
        if (*cursor == '\'') {
            char* strStart = cursor++;

            char c = *cursor;
            if (c == '\\') {
                cursor++;

                switch (*cursor) {
                    case 'n': c = '\n'; break;
                    case 't': c = '\t'; break;
                    case '\\': c = '\\'; break;
                    case '\'': c = '\''; break;
                }

            } else {
                if (c > '~' || c < ' ') error(current_line, get_current_file(parser)->filename, "Invalid character.");
            }

            cursor++;
            if (*cursor != '\'') error(current_line, get_current_file(parser)->filename, "Missing closing single quote.");

            Token token = {
                .type = Tok_Char,
                .line = current_line,
                .data.character = c
            };
            list_add(parser->tokens, token);
            continue;
        }

        // string
        if (*cursor == '"') {
            char* strStart = cursor + 1;
            while ( !(*++cursor == '"' && *(cursor - 1) != '\\') ) {
                if (*cursor == '\n') {
                    // TODO: consider whether we want to allow multi-line strings
                    error(current_line, get_current_file(parser)->filename, "Strings cannot contain new-lines.");
                    current_line++;
                }
            }

            StrSpan str = {
                .start = strStart,
                .length = cursor - strStart
            };

            Token token = {
                .type = Tok_String,
                .line = current_line,
                .data.string = register_string(str)
            };

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
                if (*cursor == '\n') current_line++;
            }
            cursor++;
            continue;
        }


        switch (*cursor) {

            case ',': appendToken(parser, Tok_Comma); continue;

            case '.': switch (*(cursor + 1)) {
                case '.': appendToken(parser, Tok_Dotdot); cursor++; continue;
                default: appendToken(parser, Tok_Period); continue;
            }

            case ';': appendToken(parser, Tok_Semicolon); continue;
            case ':': appendToken(parser, Tok_Colon); continue;
            case '?': appendToken(parser, Tok_QuestionMark); continue;
            case '@': appendToken(parser, Tok_At); continue;
            case '~': appendToken(parser, Tok_Tilde); continue;

            case '&': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_BitAndAssign); cursor++; continue;
                default: appendToken(parser, Tok_Ampersand); continue;
            }
            case '|': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_BitOrAssign); cursor++; continue;
                default: appendToken(parser, Tok_Pipe); continue;
            }
            case '^': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_BitXorAssign); cursor++; continue;
                default: appendToken(parser, Tok_Caret); continue;
            }

            case '{': appendToken(parser, Tok_OpenCurl); continue;
            case '}': appendToken(parser, Tok_CloseCurl); continue;
            case '(': appendToken(parser, Tok_OpenParen); continue;
            case ')': appendToken(parser, Tok_CloseParen); continue;
            case '[': appendToken(parser, Tok_OpenSquare); continue;
            case ']': appendToken(parser, Tok_CloseSquare); continue;

            case '<': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_LessThanOrEqual); cursor++; continue;
                case '<': appendToken(parser, Tok_LeftShift); cursor++; continue;
                default: appendToken(parser, Tok_LessThan); continue;
            }

            case '>': switch (*(cursor + 1)) {
                case '=': appendToken(parser, Tok_GreaterThanOrEqual); cursor++; continue;
                case '>': appendToken(parser, Tok_RightShift); cursor++; continue;
                default: appendToken(parser, Tok_GreaterThan); continue;
            }

            case '+': switch (*(cursor + 1)) {
                case '+': appendToken(parser, Tok_PlusPlus); cursor++; continue;
                case '=': appendToken(parser, Tok_PlusAssign); cursor++; continue;
                default: appendToken(parser, Tok_Plus); continue;
            }

            case '-': switch (*(cursor + 1)) {
                case '-': appendToken(parser, Tok_MinusMinus); cursor++; continue;
                case '=': appendToken(parser, Tok_MinusAssign); cursor++; continue;
                default: appendToken(parser, Tok_Minus); continue;
            }

        }

        #define test_op_eq(c, token, tokenEqual) \
            if (*cursor == c) { \
                if (*(cursor + 1) == '=') { \
                    appendToken(parser, tokenEqual); \
                    cursor++; \
                } else appendToken(parser, token); \
                continue; \
            } \


        test_op_eq('=', Tok_Assign, Tok_Equals);
        test_op_eq('!', Tok_ExclamationMark, Tok_NotEquals);
        test_op_eq('*', Tok_Mul, Tok_MulAssign);
        test_op_eq('/', Tok_Div, Tok_DivAssign);
        test_op_eq('%', Tok_Mod, Tok_ModAssign);


        // Error: token not recognized
        error(current_line, get_current_file(parser)->filename, "Unknown byte value encountered %d.", *cursor);
    }
}
