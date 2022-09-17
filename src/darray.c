


typedef struct {
    u32 length, capacity, stride;
} DarrayHead;

DarrayHead* darrayHead(void* list) { return &((DarrayHead*)list)[-1]; }

#define foreach(item, list)\
    typeof(list) item = &list[0];\
    u32 item##_count = darrayLength(list);\
    for (u32 item##_index = 0; item##_index < item##_count; item = &list[++item##_index])


void darrayDelete(void* list) { free(darrayHead(list)); }
void darrayClear(void* list) { darrayHead(list)->length = 0; }
u32 darrayLength(void* list)   { return ((u32*)list)[-3]; }
u32 darrayCapacity(void* list) { return ((u32*)list)[-2]; }
u32 darrayStride(void* list)   { return ((u32*)list)[-1]; }

#define darrayCreate(type) _darrayCreate(sizeof(type))
void* _darrayCreate(u32 stride) {
    DarrayHead* head = malloc(
        sizeof(DarrayHead) +
        stride       // first element
    );

    void* list = &head[1];

    head->capacity = 1;
    head->length = 0;
    head->stride = stride;

    return list;
}

#define darrayAdd(list, value) {   \
    typeof(value) __unique_var_name_to_not_fuck_things_up = value;     \
    _darrayAdd((void**)&list, &__unique_var_name_to_not_fuck_things_up); \
}

void _darrayAdd(void** list, void* value) {
    DarrayHead* head = darrayHead(*list);

    if (head->length == head->capacity) {
        // resize
        head->capacity *= 2;
        head = realloc(head, sizeof(DarrayHead) + head->stride * head->capacity);
        *list = &head[1];
    }


    u64 addr = (u64)*list;
    addr += head->length * head->stride;
    memcpy((void*)addr, value, head->stride);

    //u32 bytePos = head->length * head->stride;
    //((u8*)list)[bytePos] = *value;

    head->length++;

}