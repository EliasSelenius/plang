#pragma once

#include "types.h"
#include "essh-string.h"


typedef enum TokenType {
    Tok_Whitespace,
    Tok_Word,

    Tok_Keyword_Struct,
    Tok_Keyword_Alloc,
    Tok_Keyword_Let,
    Tok_Keyword_If,
    Tok_Keyword_While,
    Tok_Keyword_True,
    Tok_Keyword_False,
    Tok_Keyword_Null,
    Tok_Keyword_Continue,
    Tok_Keyword_Break,
    Tok_Keyword_Return,

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

void lex(char* input);