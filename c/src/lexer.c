#include "lexer.h"

#include <stdio.h>

// TODO: use dynamic array 
Token tokens[100000];
u32 tokens_length = 0;

static u32 current_line;
static char* cursor;

static void appendTokenLength(TokenType type, u32 len) {
    tokens[tokens_length++] = (Token) {
        .type = type,
        .line = current_line,
        .value = (StrSpan) {
            .start = cursor,
            .length = len
        }
    };
}

static void appendToken(TokenType type) {
    appendTokenLength(type, 1);
}


void lex(char* input) {

    cursor = input - 1;

    current_line = 1;

    while (true) {
        cursor++;

        if (*cursor == '\0') break;
        
        
        { // handle whitespace
            if (*cursor == ' ') continue;

            if (*cursor == '\n') {
                current_line++;
                continue;
            }
        }

        // Word
        if (isLetter(*cursor)) {
            char* wordStart = cursor;
            while (isLetter(*++cursor));
            cursor--;

            StrSpan word = (StrSpan) {
                .start = wordStart,
                .length = cursor - (wordStart - 1)
            };

            TokenType tokType = Tok_Word;

            // Keywords
            if (spanEquals(word, "struct")) tokType = Tok_Keyword_Struct;

            tokens[tokens_length++] = (Token) {
                .type = tokType,
                .line = current_line,
                .value = word
            };

            continue;
        }

        // number
        if ( isDigit(*cursor) ||
                (isDigit(*(cursor + 1)) && *cursor == '-')
           ) {
            // integer part
            char* digitStart = cursor;
            while (isDigit(*++cursor));
            // decimal part
            if (*cursor == '.') {
                while (isDigit(*++cursor));
            }
            cursor--;

            tokens[tokens_length++] = (Token) {
                .type = Tok_Number,
                .line = current_line,
                .value = (StrSpan) {
                    .start = digitStart,
                    .length = cursor - (digitStart - 1)
                }
            };

            continue;
        }

        // string
        if (*cursor == '"') {
            char* strStart = cursor;
            while ( !(*++cursor == '"' && *(cursor - 1) != '\\') ) {
                if (*cursor == '\n') {
                    // TODO: consider whether we want to allow multi-line strings
                    printf("Error: Strings cannot contain new-lines. At line %d\n", current_line);
                    current_line++;
                }
            }

            tokens[tokens_length++] = (Token) {
                .type = Tok_String,
                .line = current_line,
                .value = (StrSpan) {
                    .start = strStart,
                    .length = cursor - (strStart - 1)
                }
            };

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
        
        test_op_eq('+', Tok_Plus, Tok_PlusEquals);
        test_op_eq('-', Tok_Minus, Tok_MinusEquals);
        test_op_eq('*', Tok_Mul, Tok_MulEquals);
        test_op_eq('/', Tok_Div, Tok_DivEquals);


        


        // Error: token not recognized
        printf("Error: Token %c is not recognized. At line %d\n", *cursor, current_line);

    }
    
}
