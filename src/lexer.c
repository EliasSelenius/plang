#include "lexer.h"
#include "darray.h"
#include "parser.h"

#include <stdio.h>
#include <math.h>

Token* tokens; // darray

static u32 current_line;
static char* cursor;

inline void appendToken(TokenType type) {
    Token token;
    token.type = type;
    token.line = current_line;
    darrayAdd(tokens, token);
}

static u64 number(u32* numDigits) {
    char* start = cursor;
    while (isDigit(*++cursor));
    u32 len = cursor - start;

    u64 acc = 0;
    u64 place = 1;
    for (i32 i = len - 1; i >= 0; i--) {
        u64 d = start[i] - '0';
        acc += place * d;
        place *= 10;
    }

    *numDigits = len;
    return acc;
}

// u32 lex2(char* input) {
//     while (true) {
//         switch (*cursor) {
//             case '\0': return 0;
//             case ' ': continue;
//             case '\n': current_line++;
//         }
//     }
// }

u32 lex(char* input) {
    u32 numberOfErrors = 0;

    cursor = input - 1;

    current_line = 1;

    while (true) {
        cursor++;



        // TODO: this should be a switch statement as it would be faster
        //       ĉi tio devus esti ŝaltilo deklaro ĉar ĝi estus pli rapida

        if (*cursor == '\0') break;


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
            else if (spanEquals(word, "alloc")) tokType = Tok_Keyword_Alloc;
            else if (spanEquals(word, "let")) tokType = Tok_Keyword_Let;
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
            else if (spanEquals(word, "declare")) tokType = Tok_Keyword_Declare;
            else if (spanEquals(word, "type")) tokType = Tok_Keyword_Type;
            else if (spanEquals(word, "as")) tokType = Tok_Keyword_As;
            else if (spanEquals(word, "const")) tokType = Tok_Keyword_Const;
            else if (spanEquals(word, "for")) tokType = Tok_Keyword_For;
            else if (spanEquals(word, "switch")) tokType = Tok_Keyword_Switch;
            else if (spanEquals(word, "case")) tokType = Tok_Keyword_Case;
            else if (spanEquals(word, "goto")) tokType = Tok_Keyword_Goto;
            else if (spanEquals(word, "with")) tokType = Tok_Keyword_With;
            else if (spanEquals(word, "namespace")) tokType = Tok_Keyword_Namespace;
            else if (spanEquals(word, "sizeof")) tokType = Tok_Keyword_Sizeof;

            Token token;
            token.type = tokType;
            token.line = current_line;

            if (token.type == Tok_Word) {
                token.stringTableByteOffset = appendStringToStringtable(word);
            }

            darrayAdd(tokens, token);

            continue;
        }

        // hex number
        if (*cursor == '0' && *(cursor + 1) == 'x') {
            char* digitStart = cursor;
            cursor++;
            while (isHexDigit(*++cursor));
            cursor--;

            Token token = {
                .type = Tok_Integer, // Tok_Integer_Uint,
                .line = current_line,
                // .value = {
                    // .start = digitStart,
                    // .length = cursor - digitStart + 1
                // }
                .integer = 0
            };
            darrayAdd(tokens, token);

            continue;
        }

        // new number
        if (isDigit(*cursor)) {
            Token token = {0};
            token.line = current_line;
            token.type = Tok_Integer;

            u32 numDigits;
            u64 num = number(&numDigits);
            token.integer = num;

            if (*cursor == '.') {
                cursor++;
                f64 fraction = (f64)number(&numDigits);

                f64 denom = 1.0;
                for (u32 i = 0; i < numDigits; i++) denom *= 10.0;

                f64 value = (f64)num + (fraction / denom);

                token.type = Tok_Decimal;
                token.decimal = value;
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

            darrayAdd(tokens, token);
            continue;
        }

        // number
        /* if ( isDigit(*cursor) ||            // TODO: dont tokenize minus as part of the number token. it fucks up things like (list[i-1]) the minus token needs to be sepperate from the number
                (isDigit(*(cursor + 1)) && *cursor == '-')
           ) {
            // integer part
            char* digitStart = cursor;
            while (isDigit(*++cursor));

            // 123.123
            u32 length = cursor - digitStart;

            TokenType tt = Tok_Integer;

            switch (*cursor) {
                case 'u': {
                    if (*(cursor + 1) == 'l') {
                        tt = Tok_Integer_Ulong;
                        cursor++;
                    } else {
                        tt = Tok_Integer_Uint;
                    }
                } break;

                case 'l': tt = Tok_Integer_Long; break;
                case 'f': tt = Tok_Decimal_Float; break;
                case 'd': tt = Tok_Decimal_Double; break;

                case '.': {
                    tt = Tok_Decimal;
                    while (isDigit(*++cursor));
                    length = cursor - digitStart;

                    if (*cursor == 'f') tt = Tok_Decimal_Float;
                    else if (*cursor == 'd') tt = Tok_Decimal_Double;
                    else cursor--;

                } break;

                default: cursor--; break;
            }

            Token token = {
                .type = tt,
                .line = current_line,
                .value = {
                    .start = digitStart,
                    .length = length
                }
            };
            darrayAdd(tokens, token);

            continue;
        } */

        // chars
        if (*cursor == '\'') {
            char* strStart = cursor++;


            if (*cursor > 0x7E || *cursor < 0x20) {
                printf("Error: Invalid character. At line %d\n", current_line);
                numberOfErrors++;
            }

            char c = *cursor;

            cursor++;
            if (*cursor != '\'') {
                printf("Error: Missing closing single quote. At line %d\n", current_line);
                numberOfErrors++;
            }

            Token token = {
                .type = Tok_Char,
                .line = current_line,
                .character = c
            };
            darrayAdd(tokens, token);
            continue;
        }

        // string
        if (*cursor == '"') {
            char* strStart = cursor;
            while ( !(*++cursor == '"' && *(cursor - 1) != '\\') ) {
                if (*cursor == '\n') {
                    // TODO: consider whether we want to allow multi-line strings
                    printf("Error: Strings cannot contain new-lines. At line %d\n", current_line);
                    numberOfErrors++;
                    current_line++;
                }
            }

            StrSpan str = {
                .start = strStart,
                .length = cursor - (strStart - 1)
            };

            Token token = {
                .type = Tok_String,
                .line = current_line,
                .stringTableByteOffset = appendStringToStringtable(str)
            };

            darrayAdd(tokens, token);
            continue;
        }

        // Line comment
        if (*cursor == '/' && *(cursor + 1) == '/') {
            char* commentStart = cursor;
            while (*++cursor != '\n');
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

            case ',': appendToken(Tok_Comma); continue;
            case '.': appendToken(Tok_Period); continue;
            case ';': appendToken(Tok_Semicolon); continue;
            case ':': appendToken(Tok_Colon); continue;
            case '?': appendToken(Tok_QuestionMark); continue;
            case '@': appendToken(Tok_At); continue;
            case '~': appendToken(Tok_Tilde); continue;
            case '&': appendToken(Tok_Ampersand); continue;
            case '|': appendToken(Tok_Pipe); continue;
            case '^': appendToken(Tok_Caret); continue;
            case '{': appendToken(Tok_OpenCurl); continue;
            case '}': appendToken(Tok_CloseCurl); continue;
            case '(': appendToken(Tok_OpenParen); continue;
            case ')': appendToken(Tok_CloseParen); continue;
            case '[': appendToken(Tok_OpenSquare); continue;
            case ']': appendToken(Tok_CloseSquare); continue;

            case '<': switch (*(cursor + 1)) {
                case '=': appendToken(Tok_LessThanOrEqual); cursor++; continue;
                case '<': appendToken(Tok_LeftShift); cursor++; continue;
                default: appendToken(Tok_LessThan); continue;
            }

            case '>': switch (*(cursor + 1)) {
                case '=': appendToken(Tok_GreaterThanOrEqual); cursor++; continue;
                case '>': appendToken(Tok_RightShift); cursor++; continue;
                default: appendToken(Tok_GreaterThan); continue;
            }

        }

        #define test_op_eq(c, token, tokenEqual) \
            if (*cursor == c) { \
                if (*(cursor + 1) == '=') { \
                    appendToken(tokenEqual); \
                    cursor++; \
                } else appendToken(token); \
                continue; \
            } \


        test_op_eq('=', Tok_Assign, Tok_Equals);
        test_op_eq('!', Tok_ExclamationMark, Tok_NotEquals);

        if (*cursor == '+') {
            switch (*(cursor + 1)) {
                case '+': appendToken(Tok_PlusPlus); cursor++; break;
                case '=': appendToken(Tok_PlusAssign); cursor++; break;
                default: appendToken(Tok_Plus); break;
            }
            continue;
        }

        if (*cursor == '-') {
            switch (*(cursor + 1)) {
                case '-': appendToken(Tok_MinusMinus); cursor++; break;
                case '=': appendToken(Tok_MinusAssign); cursor++; break;
                default: appendToken(Tok_Minus); break;
            }
            continue;
        }

        // test_op_eq('+', Tok_Plus, Tok_PlusAssign);
        // test_op_eq('-', Tok_Minus, Tok_MinusAssign);
        test_op_eq('*', Tok_Mul, Tok_MulAssign);
        test_op_eq('/', Tok_Div, Tok_DivAssign);
        test_op_eq('%', Tok_Mod, Tok_ModAssign);


        // Error: token not recognized
        printf("Error: Unknown byte value encountered %d. At line %d\n", *cursor, current_line);
        numberOfErrors++;
    }

    return numberOfErrors;
}
