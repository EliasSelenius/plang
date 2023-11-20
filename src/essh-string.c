
StrSpan spFrom(char* str) {
    return (StrSpan) {
        .length = strlen(str),
        .start = str
    };
}

void spCopyTo(StrSpan src, char* dest) {
    for (u32 i = 0; i < src.length; i++) dest[i] = src.start[i];
    // TODO: should we null terminate dest?
}

StrSpan spTrim(StrSpan sv) {
    return spTrimStart(spTrimEnd(sv));
}

StrSpan spTrimStart(StrSpan sv) {
    while (isWhitespace(*sv.start)) {
        sv.start++;
        sv.length--;
    }
    return sv;
}

StrSpan spTrimEnd(StrSpan sv) {
    char* end = sv.start + sv.length;
    while (isWhitespace(*--end));
    sv.length = end - sv.start + 1;
    return sv;
}

StrSpan spTrimChar(StrSpan sv, char c) {
    return spTrimCharStart(spTrimCharEnd(sv, c), c);
}

StrSpan spTrimCharStart(StrSpan sv, char c) {
    while (*sv.start == c) {
        sv.start++;
        sv.length--;
    }
    return sv;
}

StrSpan spTrimCharEnd(StrSpan sv, char c) {
    char* end = sv.start + sv.length;
    while ((*--end) == c);
    sv.length = end - sv.start + 1;
    return sv;
}


bool spStartsWith(StrSpan sv, char* start) {
    u32 i = 0;
    while (true) {
        if (start[i] == '\0') return true;
        if (sv.start[i] == '\0') return false;
        if (start[i] != sv.start[i]) return false;
        i++;
    }
}



bool spanEquals(StrSpan span, char* str) {
    return spanEqualsSpan(span, spFrom(str));
}

bool spanEqualsSpan(StrSpan span1, StrSpan span2) {
    if (span1.length != span2.length) return false;

    for (u32 i = 0; i < span1.length; i++) {
        if (span1.start[i] != span2.start[i]) return false;
    }

    return true;
}

bool cstrEquals(char* a, char* b) {
    u32 i = 0;
    while (true) {
        if (a[i] != b[i]) return false;
        if (a[i] == '\0') return true;
        i++;
    }
}

static char number_string[20]; // 20 is max char size for 64 bit integer
StrSpan numberToString(u64 num) {
    if (num == 0) return (StrSpan) { "0", 1 };
    u32 strIndex = 20;

    while (num != 0) {
        u64 r = num % 10; num /= 10;
        number_string[--strIndex] = r + '0';
    }

    return (StrSpan) { &number_string[strIndex], 20 - strIndex };
}


// ----------StringBuilder----------------

StringBuilder sbCreateWithCapacity(u32 initialCapacity) {
    StringBuilder sb;
    sb.capacity = initialCapacity;
    sb.length = 0;
    sb.content = malloc(sb.capacity);
    return sb;
}

StringBuilder sbCreate() {
    return sbCreateWithCapacity(16);
}

void sbDestroy(StringBuilder* sb) {
    free(sb->content);
    sb->content = null;
    sb->capacity = sb->length = 0;
}

static void growBufferSize(StringBuilder* sb, u32 additionalSpace) {
    u32 newLength = sb->length + additionalSpace;

    // NOTE: we do <= (as opposed to only <) to make sure there is space for zero termination.
    if (sb->capacity <= newLength) {
        sb->capacity *= 2;

        // make sure there is sufficent storage space
        while (sb->capacity <= newLength)
            sb->capacity *= 2;

        sb->content = realloc(sb->content, sb->capacity);
    }
}

void sbAppendChar(StringBuilder* sb, char c) {
    growBufferSize(sb, 1);
    sb->content[sb->length++] = c;
    // make sure the content is zero-terminated, so that it can be used as a c string
    sb->content[sb->length] = '\0';
}

void sbAppend(StringBuilder* sb, char* str) {
    u32 strLen = strlen(str);
    growBufferSize(sb, strLen);

    u32 i = 0;
    while (true) {
        if (str[i] == '\0') break;
        sb->content[sb->length++] = str[i];
        i++;
    }

    // make sure the content is zero-terminated, so that it can be used as a c string
    sb->content[sb->length] = '\0';
}

void sbAppendSpan(StringBuilder* sb, StrSpan str) {
    growBufferSize(sb, str.length);

    for (int i = 0; i < str.length; i++) {
        sb->content[sb->length++] = str.start[i];
    }

    // make sure the content is zero-terminated, so that it can be used as a c string
    sb->content[sb->length] = '\0';
}

void sbCopyIntoBuffer(StringBuilder* sb, char* buffer, u32 bufferLength) {
    for (int i = 0; i < sb->length && i < bufferLength; i++) {
        buffer[i] = sb->content[i];
    }
}

StringBuilder* temp_builder() {

    static u32 calls = 0;
    static const u32 builders_count = 2;
    static StringBuilder builders[builders_count] = {0};

    StringBuilder* sb = &builders[calls++ % builders_count];

    if (sb->content == null) *sb = sbCreate();
    sbClear(sb);

    return sb;
}