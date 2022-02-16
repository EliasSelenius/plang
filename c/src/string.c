#include "string.h"
#include "types.h"

bool spanEquals(StrSpan span, char* str) {
    
    for (u32 i = 0; i < span.length; i++) {
        if (str[i] == '\0') return false;
        if (span.start[i] != str[i]) return false;
    }

    return true;
}

bool spanEqualsSpan(StrSpan span1, StrSpan span2) {
    if (span1.length != span2.length) return false;

    for (u32 i = 0; i < span1.length; i++) {
        if (span1.start[i] != span2.start[i]) return false;
    }

    return true;
}