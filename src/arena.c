
#define arena_size 0x100000000

typedef struct Arena {
    u32 top;
} Arena;

Arena* arena_create() {
    Arena* arena = vmem_reserve(arena_size);
    vmem_commit(arena, arena_size);
    arena->top = sizeof(Arena);
    return arena;
}

void* arena_alloc(Arena* arena, u32 size) {
    void* res = arena + arena->top;
    arena->top += size;
    return res;
}

// void arena_reset(Arena* arena) {
//     // TODO: this decommit then commit thing am not sure about
//     vmem_decommit(arena, arena->top);
//     vmem_commit(arena, arena_size);
//     arena->top = 0;
// }

void arena_release(Arena* arena) {
    vmem_release(arena);
}
