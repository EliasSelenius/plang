
#define arena_size 0x1000000

typedef struct Arena {
    u32 top;
} Arena;

Arena* arena_create() {
    // Arena* arena = vmem_reserve(arena_size);
    // if (vmem_commit(arena, arena_size) == null) {
    //     u32 error = GetLastError();
    //     printf("vmem_commit() error, with code %u\n", error);
    // }

    Arena* arena = vmem_reserve_and_commit(arena_size);
    arena->top = sizeof(Arena);
    return arena;
}

void* arena_alloc(Arena* arena, u32 size) {
    void* res = (void*)((u64)arena + (u64)arena->top);
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
