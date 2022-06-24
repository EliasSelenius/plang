#pragma once

#include "types.h"

// TODO: make a string library

typedef struct StrSpan {
    char* start;
    u32 length;
} StrSpan;


StrSpan spFrom(char* str);
void spCopyTo(StrSpan src, char* dest);

StrSpan spTrim(StrSpan sv);
StrSpan spTrimStart(StrSpan sv);
StrSpan spTrimEnd(StrSpan sv);

StrSpan spTrimChar(StrSpan sv, char c);
StrSpan spTrimCharStart(StrSpan sv, char c);
StrSpan spTrimCharEnd(StrSpan sv, char c);

bool spStartsWith(StrSpan sv, char* start);



inline bool isWhitespace(char c) { return c == ' ' || c == '\n'; }
inline bool isUpperCaseLetter(char c) { return c >= 'A' && c <= 'Z'; }
inline bool isLowerCaseLetter(char c) { return c >= 'a' && c <= 'z'; }
inline bool isLetter(char c) { return isLowerCaseLetter(c) || isUpperCaseLetter(c); }
inline bool isDigit(char c) { return c >= '0' && c <= '9'; }
inline bool isHexDigit(char c) { return isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }

bool spanEquals(StrSpan span, char* str);
bool spanEqualsSpan(StrSpan span1, StrSpan span2);



// --------StringBuilder------------------

typedef struct StringBuilder {
    u32 length;
    u32 capacity;
    char* content;
} StringBuilder;


StringBuilder sbCreateWithCapacity(u32 initialCapacity);
StringBuilder sbCreate();
void sbDestroy(StringBuilder* sb);
inline void sbClear(StringBuilder* sb) { sb->length = 0; }
void sbAppendChar(StringBuilder* sb, char c);
void sbAppend(StringBuilder* sb, char* str);
void sbAppendSpan(StringBuilder* sb, StrSpan str);
void sbCopyIntoBuffer(StringBuilder* sb, char* buffer, u32 bufferLength);

