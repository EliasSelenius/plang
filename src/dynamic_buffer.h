#pragma once

#include "types.h"

typedef struct DynamicBuffer {
    u32 length;
    u32 capacity;
    u8 bytes[];
} DynamicBuffer;


DynamicBuffer* dyCreate();
u32 dyReserve(DynamicBuffer** buffer, u32 size);
inline void dyClear(DynamicBuffer* buffer) { buffer->length = 0; }