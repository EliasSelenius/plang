
#include <stdio.h>
#include "Windows.h"

typedef unsigned int uint32;
#define null 0

static uint32 roundup2multiple(uint32 num, uint32 multiple) {
    return (uint32)((float)num / (float)multiple + 1) * multiple;
}


#define arena_size 0xFFFFFFFF

SYSTEM_INFO info;

typedef struct Arena {
    void* data;
    uint32 allocated;
} Arena;

Arena* test_arena = &(Arena) {0};

Arena arena_create() {
    Arena res;
    res.data = VirtualAlloc(null, arena_size, MEM_RESERVE, PAGE_NOACCESS);
    res.allocated = 0;
    return res;
}

void* arena_alloc(Arena* arena, uint32 size) {
    void* res = (void*)(arena->data + arena->allocated);
    arena->allocated += size;

    VirtualAlloc(arena->data, arena->allocated, MEM_COMMIT, PAGE_READWRITE);

    return res;
}

void arena_reset(Arena* arena) {
    VirtualFree(arena->data, arena->allocated, MEM_DECOMMIT);
    arena->allocated = 0;
}

void arena_destroy(Arena* arena) {
    VirtualFree(arena->data, 0, MEM_RELEASE);
    arena->data = null;
    arena->allocated = 0;
}

int main() {

    GetSystemInfo(&info);
    printf("Alloc gran.: %lu\nPagesize: %lu\n", info.dwAllocationGranularity, info.dwPageSize);

    uint32 i = 8000;
    printf("Rounding test: %d -> %d\n", i, roundup2multiple(i, info.dwPageSize));

    *test_arena = arena_create();
    char* buffer1 = arena_alloc(test_arena, 3000);
    buffer1[0] = 'E';
    char* buffer2 = arena_alloc(test_arena, 2000);
    buffer2[1500] = 'F';

    arena_reset(test_arena);
    void* new_buffer = arena_alloc(test_arena, 12);

    if (new_buffer == buffer1) {
        printf("They are equal\n");
    } else printf("They are not equal\n");

    arena_destroy(test_arena);

    printf("Done\n");
    return 0;
}