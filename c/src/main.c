#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef char bool;
#define true 1
#define false 0

typedef unsigned int u32;
typedef unsigned char u8;

typedef enum TokenType {
    Tok_Whitespace,
    Tok_Word,

    Tok_Number,
    Tok_String,

    Tok_Comment,
    Tok_MultiComment,

    Tok_Comma,
    Tok_Period,
    Tok_Semicolon,

    Tok_OpenCurl,
    Tok_CloseCurl,
    Tok_OpenParen,
    Tok_CloseParen,
    Tok_OpenSquare,
    Tok_CloseSquare,

    Tok_LessThan,
    Tok_GreaterThan,
    Tok_LessThanOrEqual,
    Tok_GreaterThanOrEqual,
    Tok_Equals,
    Tok_Assign,

    Tok_Plus,
    Tok_Minus,
    Tok_Mul,
    Tok_Div,

    Tok_PlusEquals,
    Tok_MinusEquals,
    Tok_MulEquals,
    Tok_DivEquals

} TokenType;

char* TokenTypeNames[] = {
    [Tok_Whitespace] = "Whitespace"
};

inline bool isWhitespace(char c) {
    return c == ' ' || c == '\n';
}

inline bool isUpperCaseLetter(char c) {
    return c >= 'A' && c <= 'Z';
}

inline bool isLowerCaseLetter(char c) {
    return c >= 'a' && c <= 'z';
}

inline bool isLetter(char c) {
    return isLowerCaseLetter(c) || isUpperCaseLetter(c);
}

inline bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

typedef struct StrSpan {
    char* start;
    u32 length;
} StrSpan;

typedef struct Token {
    TokenType type;
    StrSpan value;
    u32 line;
} Token;

Token tokens[100000];
u32 tokens_length = 0;

u32 current_line;
char* cursor;

void appendTokenLength(TokenType type, u32 len) {
    tokens[tokens_length++] = (Token) {
        .type = type,
        .line = current_line,
        .value = (StrSpan) {
            .start = cursor,
            .length = len
        }
    };
}

void appendToken(TokenType type) {
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

            tokens[tokens_length++] = (Token) {
                .type = Tok_Word,
                .line = current_line,
                .value = (StrSpan) {
                    .start = wordStart,
                    .length = cursor - (wordStart - 1)
                }
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

            tokens[tokens_length++] = (Token) {
                .type = Tok_Comment,
                .line = current_line,
                .value = (StrSpan) {
                    .start = commentStart,
                    .length = cursor - (commentStart - 1)
                }
            };


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

            tokens[tokens_length++] = (Token) {
                .type = Tok_MultiComment,
                .line = current_line,
                .value = (StrSpan) {
                    .start = commentStart,
                    .length = cursor - (commentStart - 1)
                }
            };

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


u32 token_index;


typedef struct PlangType {
    StrSpan structName;
    u8 numPointers;
} PlangType;

typedef struct PlangFunction {
    StrSpan name;
    PlangType returnType;
} PlangFunction;

typedef enum ExprType {
    ExprType_Number,
    ExprType_Variable,
    ExprType_Arithmetic
} ExprType;

typedef struct Expression {
    ExprType expressionType;
    union {
        StrSpan* value;
        struct {
            u32 count;
            StrSpan** operator;
            struct Expression** values;
        };
    };
} Expression;

void printExpression(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Variable:
        case ExprType_Number: 
            printf("%.*s", expr->value->length, expr->value->start); 
            break;

        case ExprType_Arithmetic: {
            for (u32 i = 0; i < expr->count; i++) {
                printExpression(expr->values[i]);
                printf(" / ");
            }
        }
    }
}


// returns false if the token can not be interpreted as a type
bool parseType(PlangType* type) {
    if (tokens[token_index].type == Tok_Word) {
        
        type->structName = tokens[token_index].value;
        token_index++;

        u8 np = 0;
        while (tokens[token_index++].type == Tok_Mul) np++;
        type->numPointers = np;
        token_index--;

        return true;
    }

    return false;
}

#define assertToken(tok) if (tokens[token_index++].type != tok) goto failCase;

#define nextToken(tok) (tokens[token_index++].type == tok)

bool parseFunction() {
    u32 startingIndex = token_index;

    PlangFunction func;

    // type
    if (!parseType(&func.returnType)) goto failCase;

    // name
    if (tokens[token_index++].type != Tok_Word) goto failCase;    
    func.name = tokens[token_index].value;

    // args
    assertToken(Tok_OpenParen)
    assertToken(Tok_CloseParen)

    // body
    assertToken(Tok_OpenCurl)
    assertToken(Tok_CloseCurl)

    return true;
failCase:
    token_index = startingIndex;
    return false;
}


inline bool isOperator(TokenType type) {
    return type >= Tok_Plus && type <= Tok_Div;
}

Expression* parseLeafExpression() {
    Token* token = &tokens[token_index++];
    ExprType eType;

    switch (token->type) {
        case Tok_Word: {
            eType = ExprType_Variable;
        } break;
        case Tok_Number: {
            eType = ExprType_Number;
        } break;

        case Tok_OpenParen: {
            // TODO: parse sub expr
        } break;

        default: {
            token_index--;
            return NULL;
        }
    }

    Expression* expr = malloc(sizeof(Expression));
    expr->value = &token->value;
    expr->expressionType = eType;
    return expr;
}

Expression* parseExpression() {
    u32 startingIndex = token_index;

    Expression* leafExpr = parseLeafExpression();
    if (!leafExpr) goto failCase;

    u32 i = 0;
    Expression* temp[16];
    temp[i++] = leafExpr;

    while (isOperator(tokens[token_index].type)) {
        token_index++;
        temp[i++] = parseLeafExpression();
    }

    if (i == 1) return leafExpr;


    Expression* expr = malloc(sizeof(Expression));
    expr->expressionType = ExprType_Arithmetic;
    expr->count = i;
    expr->values = malloc(sizeof(Expression) * i);
    for (u32 j = 0; j < expr->count; j++) {
        expr->values[j] = temp[j];
    }

    // while (i > 0) expr->values[--i] = temp[i];

    return expr;


failCase:
    token_index = startingIndex;
    return NULL;
}

/*
    parse expression 1
        - arithmetic
            - leaf (op leaf)+
        - leaf

    parse expression 2 
        - leaf
        - if op

Expression* parseExpression() {
    u32 startingIndex = token_index;

    Expression* expr;
    if ( (expr = parseArithmeticExpression()) ) return expr;

    if ( (expr = parseLeafExpression()) ) return expr;

    token_index = startingIndex;
    return NULL;
}
*/

void parse() {
    for (token_index = 0; token_index < tokens_length; token_index++) {

    }
}


char* fileread(const char* filename, u32* strLength) {
    FILE* file = fopen(filename, "r");

    if (file == NULL) {
        printf("Could not read file: %s\n", filename);
        perror("fileread() error");
    }

    fseek(file, 0, SEEK_END);
    *strLength = ftell(file);
    rewind(file);


    char* res = calloc(*strLength + 1, sizeof(char));
    fread(res, 1, *strLength, file);

    //printf("file:%s has length: %d, and strlen = %llu\n", filename, *strLength, strlen(res));

    fclose(file);

    return res;
}

Expression* gExpressions[16];
u32 exprIndex = 0;

int main(int argc, char* argv[]) {

    // char text[] = "void main() }  Hello\n    12\nWorld!\n    DAw\nvoid main() {\n    let x = 12;\n    x += 10;\n    if (x == 12 || x <= 12) return;}";

    u32 filesize;
    char* text = fileread("lexTest.txt", &filesize);
    // char* text = fileread("src/main.c", &filesize);


    printf("Start lexing\n");
    lex(text);
    printf("End lexing\n");

    for (u32 i = 0; i < tokens_length; i++) {
        Token token = tokens[i];

        printf("Token %d: |%.*s|\n", token.type, token.value.length, token.value.start);
    }

    printf("No more tokens\n");

    printf("Start Parsing...\n");

    while ( (gExpressions[exprIndex++] = parseExpression()) );

    if (parseFunction()) {
        printf("success\n");
    } else {
        printf("succesfully failed\n");
    }

    return 0;
}