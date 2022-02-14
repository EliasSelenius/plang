#pragma once

#include "types.h"

typedef struct StrSpan {
    char* start;
    u32 length;
} StrSpan;



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

/*
char* TokenTypeNames[] = {
    [Tok_Whitespace] = "Whitespace"
};
*/

typedef struct Token {
    TokenType type;
    StrSpan value;
    u32 line;
} Token;

extern Token tokens[100000];
extern u32 tokens_length;

inline bool isWhitespace(char c) { return c == ' ' || c == '\n'; }
inline bool isUpperCaseLetter(char c) { return c >= 'A' && c <= 'Z'; }
inline bool isLowerCaseLetter(char c) { return c >= 'a' && c <= 'z'; }
inline bool isLetter(char c) { return isLowerCaseLetter(c) || isUpperCaseLetter(c); }
inline bool isDigit(char c) { return c >= '0' && c <= '9'; }

void lex(char* input);