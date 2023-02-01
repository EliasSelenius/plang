
#define arena_size 0xFFFFFFFF

typedef struct Arena {
    void* data;
    u32 allocated;
} Arena;

Arena arena_create() {
    Arena res;
    res.data = vmem_reserve(arena_size);
    res.allocated = 0;
    return res;
}

void* arena_alloc(Arena* arena, u32 size) {
    void* res = (void*)(arena->data + arena->allocated);
    arena->allocated += size;
    vmem_commit(arena->data, arena->allocated);
    return res;
}

void arena_reset(Arena* arena) {
    vmem_decommit(arena->data, arena->allocated);
    arena->allocated = 0;
}

void arena_destroy(Arena* arena) {
    vmem_release(arena->data);
    arena->data = null;
    arena->allocated = 0;
}
