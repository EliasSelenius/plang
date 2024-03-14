
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

bool string_ends_with(char* str, char* substr) {
    return false;
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

char* alloc_string_copy(char* str) {
    u32 len = strlen(str);
    char* res = malloc(len + 1);
    strcpy_s(res, len + 1, str);
    return res;
}

char* alloc_vprintf(const char* format, va_list args) {

    va_list args2;
    va_copy(args2, args);

    int n = vsnprintf(null, 0, format, args);
    char* buffer = malloc(n + 1);

    n = vsnprintf(buffer, n + 1, format, args2);
    buffer[n] = 0;

    va_end(args2);
    return buffer;
}

char* alloc_printf(const char* format, ...) {
    va_list args;
    va_start(args, format);
    char* buffer = alloc_vprintf(format, args);
    va_end(args);
    return buffer;
}

// Levenshtein edit distance
//   naive implementation
//   TODO: maybe implement Wagner-Fischer algorithm
string lev_dist_tail(string str) {
    return (string) { str.chars + 1, str.length - 1 };
}
u32 lev_dist(string a, string b) {

    if (a.length == 0) return b.length;
    if (b.length == 0) return a.length;

    if (a.chars[0] == b.chars[0]) return lev_dist(lev_dist_tail(a), lev_dist_tail(b));

    u32 i = lev_dist(lev_dist_tail(a), b);
    u32 temp = lev_dist(a, lev_dist_tail(b));
    if (temp < i) i = temp;
    temp = lev_dist(lev_dist_tail(a), lev_dist_tail(b));
    if (temp < i) i = temp;

    return i + 1;
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


void sb_append_format_args(StringBuilder* sb, const char* format, va_list args) {
    va_list args2;
    va_copy(args2, args);
    int n = vsnprintf(null, 0, format, args);
    growBufferSize(sb, n);

    n = vsnprintf(sb->content + sb->length, n + 1, format, args2);
    sb->length += n;
    sb->content[sb->length] = '\0';

    va_end(args2);
}

void sb_append_format(StringBuilder* sb, const char* format, ...) {
    va_list args;
    va_start(args, format);
    sb_append_format_args(sb, format, args);
    va_end(args);
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




char* path_get_file_name(char* path) {
    char* res = path;
    for (u32 i = 0; path[i]; i++) {
        if (path[i] == '/') res = &path[i + 1];
    }
    return res;
}

void path_append(StringBuilder* sb, char* path) {
    if (sb->content[sb->length - 1] != '/') sbAppendChar(sb, '/');
    sbAppend(sb, path);
}