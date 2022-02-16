#pragma once

#include "types.h"

// TODO: make a string library

typedef struct StrSpan {
    char* start;
    u32 length;
} StrSpan;

inline bool isWhitespace(char c) { return c == ' ' || c == '\n'; }
inline bool isUpperCaseLetter(char c) { return c >= 'A' && c <= 'Z'; }
inline bool isLowerCaseLetter(char c) { return c >= 'a' && c <= 'z'; }
inline bool isLetter(char c) { return isLowerCaseLetter(c) || isUpperCaseLetter(c); }
inline bool isDigit(char c) { return c >= '0' && c <= '9'; }

bool spanEquals(StrSpan span, char* str);
bool spanEqualsSpan(StrSpan span1, StrSpan span2);