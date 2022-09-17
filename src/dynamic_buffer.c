
typedef struct DynamicBuffer {
    u32 length;
    u32 capacity;
    u8 bytes[];
} DynamicBuffer;

inline void dyClear(DynamicBuffer* buffer) { buffer->length = 0; }

DynamicBuffer* dyCreate() {
    u32 cap = 16;//1024;
    DynamicBuffer* buffer = malloc(sizeof(DynamicBuffer) + cap);
    buffer->length = 0;
    buffer->capacity = cap;

    return buffer;
}

u32 dyReserve(DynamicBuffer** buffer, u32 size) {
    u32 cap = (*buffer)->capacity;
    u32 oldlen = (*buffer)->length;
    u32 newlen = oldlen + size;
    (*buffer)->length = newlen;

    if (newlen > cap) {
        cap *= 2;
        while (newlen > cap) cap *= 2;
        (*buffer)->capacity = cap;
        *buffer = realloc(*buffer, sizeof(DynamicBuffer) + cap);
    }

    return oldlen;
}