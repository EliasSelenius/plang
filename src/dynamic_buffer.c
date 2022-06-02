#include "dynamic_buffer.h"

#include <stdlib.h> // malloc

DynamicBuffer* dyCreate() {
    u32 cap = 16;
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
        *buffer = realloc(*buffer, cap);
        (*buffer)->capacity = cap;
    }

    return oldlen;
}