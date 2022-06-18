#include "lexer.h"
#include "darray.h"

#include <stdio.h>

Token* tokens; // darray

static u32 current_line;
static char* cursor;

inline void appendTokenLength(TokenType type, u32 len) {
    Token token;
    token.type = type;
    token.line = current_line;
    token.value.start = cursor;
    token.value.length = len;    
    darrayAdd(tokens, token);
}

inline void appendToken(TokenType type) {
    appendTokenLength(type, 1);
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

            Token token;
            token.type = tokType;
            token.line = current_line;
            token.value = word;
            darrayAdd(tokens, token);

            continue;
        }

        // number
        if ( isDigit(*cursor) ||
                (isDigit(*(cursor + 1)) && *cursor == '-')
           ) {
            // integer part
            char* digitStart = cursor;
            while (isDigit(*++cursor));

            // 123.123
            u32 length = cursor - digitStart;

            /*
                f - float
                d - double
                u - uint
                l - long
                ul - ulong
            */

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

            Token token = {
                .type = Tok_String,
                .line = current_line,
                .value = {
                    .start = strStart,
                    .length = cursor - (strStart - 1)
                }
            };
            darrayAdd(tokens, token);
            continue;
        }

        // Line comment
        if (*cursor == '/' && *(cursor + 1) == '/') {
            char* commentStart = cursor;
            while (*++cursor != '\n');
            cursor--;

            /*
            tokens[tokens_length++] = (Token) {
                .type = Tok_Comment,
                .line = current_line,
                .value = (StrSpan) {
                    .start = commentStart,
                    .length = cursor - (commentStart - 1)
                }
            };
            */


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

            /*
            tokens[tokens_length++] = (Token) {
                .type = Tok_MultiComment,
                .line = current_line,
                .value = (StrSpan) {
                    .start = commentStart,
                    .length = cursor - (commentStart - 1)
                }
            };
            */

            continue;
        }


        #define test_char(c, t) if (*cursor == c) { appendToken(t);    continue; }
        
        test_char(',', Tok_Comma);
        test_char('.', Tok_Period);
        test_char(';', Tok_Semicolon);
        test_char(':', Tok_Colon);
        test_char('?', Tok_QuestionMark);
        test_char('@', Tok_At);

        test_char('{', Tok_OpenCurl);
        test_char('}', Tok_CloseCurl);
        test_char('(', Tok_OpenParen);
        test_char(')', Tok_CloseParen);
        test_char('[', Tok_OpenSquare);
        test_char(']', Tok_CloseSquare);


        #define test_op_eq(c, token, tokenEqual) \
            if (*cursor == c) { \
                if (*(cursor + 1) == '=') { \
                    appendTokenLength(tokenEqual, 2); \
                    cursor++; \
                } else appendToken(token); \
                continue; \
            } \


        test_op_eq('<', Tok_LessThan, Tok_LessThanOrEqual);
        test_op_eq('>', Tok_GreaterThan, Tok_GreaterThanOrEqual);

        test_op_eq('=', Tok_Assign, Tok_Equals);        
        test_op_eq('!', Tok_ExclamationMark, Tok_NotEquals);

        test_op_eq('+', Tok_Plus, Tok_PlusAssign);
        test_op_eq('-', Tok_Minus, Tok_MinusAssign);
        test_op_eq('*', Tok_Mul, Tok_MulAssign);
        test_op_eq('/', Tok_Div, Tok_DivAssign);


        


        // Error: token not recognized
        printf("Error: Token %c is not recognized. At line %d\n", *cursor, current_line);
        numberOfErrors++;
    }
    
    return numberOfErrors;
}
