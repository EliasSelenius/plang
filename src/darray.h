#pragma once
#include "types.h"


typedef struct {
    u32 length, capacity, stride;
} DarrayHead;

DarrayHead* darrayHead(void* list);

void* _darrayCreate(u32 stride);
#define darrayCreate(type) _darrayCreate(sizeof(type))
void darrayDelete(void* list);

void _darrayAdd(void** list, void* value); 
#define darrayAdd(list, value) {   \
    typeof(value) __unique_var_name_to_not_fuck_things_up = value;     \
    _darrayAdd((void**)&list, &__unique_var_name_to_not_fuck_things_up); \
}


void darrayClear(void* list);


u32 darrayLength(void* list);
u32 darrayCapacity(void* list);
u32 darrayStride(void* list);


#define foreach(item, list)\
    typeof(list) item = &list[0];\
    u32 item##_count = darrayLength(list);\
    for (u32 item##_index = 0; item##_index < item##_count; item = &list[++item##_index])