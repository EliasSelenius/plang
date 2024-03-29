


typedef struct {
    u32 length, capacity, stride;
} ListHead;

ListHead* list_head(void* list) { return &((ListHead*)list)[-1]; }

#define foreach(item, list)\
    typeof(list) item = &list[0];\
    u32 item##_count = list_length(list);\
    for (u32 item##_index = 0; item##_index < item##_count; item = &list[++item##_index])


void list_delete(void* list) { free(list_head(list)); }
void list_clear(void* list) { list_head(list)->length = 0; }
u32 list_length(void* list)   { return ((u32*)list)[-3]; }
u32 list_capacity(void* list) { return ((u32*)list)[-2]; }
u32 list_stride(void* list)   { return ((u32*)list)[-1]; }

#define list_create(type) _list_create(sizeof(type))
#define list_init(list) do { (list) = list_create(typeof(*(list))); } while (0)
void* _list_create(u32 stride) {
    ListHead* head = malloc(
        sizeof(ListHead) +
        stride       // first element
    );

    void* list = &head[1];

    head->capacity = 1;
    head->length = 0;
    head->stride = stride;

    return list;
}

#define list_add(list, value) do {   \
    typeof(value) temp = value;      \
    _list_add((void**)&list, &temp); \
} while (0)

#define list_add_empty(list) _list_add((void**)&list, null);


void* _list_add(void** list, void* value) {
    ListHead* head = list_head(*list);

    if (head->length == head->capacity) {
        // resize
        head->capacity *= 2;
        head = realloc(head, sizeof(ListHead) + head->stride * head->capacity);
        *list = &head[1];
    }

    u64 addr = (u64)*list;
    addr += head->length * head->stride;
    if (value) memcpy((void*)addr, value, head->stride);

    head->length++;

    return (void*)addr;
}

#define list_pop(list) list[--list_head(list)->length];

