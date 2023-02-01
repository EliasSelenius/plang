
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <Windows.h>

#define true 1
#define false 0

typedef unsigned int uint32;

SYSTEM_INFO sys_info;

void* memory_reserve(uint32 num_reservations) {
    return VirtualAlloc(NULL, sys_info.dwAllocationGranularity * num_reservations, MEM_RESERVE, PAGE_READWRITE);
}

void* memory_commit(void* reservated, uint32 num_pages) {
    return VirtualAlloc(reservated, sys_info.dwPageSize * num_pages, MEM_COMMIT, PAGE_READWRITE);
}

int main() {

    GetSystemInfo(&sys_info);
    printf("Pagesize:               %lu\n"
           "Allocation Granularity: %lu\n",
           sys_info.dwPageSize,
           sys_info.dwAllocationGranularity);


    char* string = "Hello, Allocator";
    int len = strlen(string);

    SIZE_T size = sys_info.dwPageSize;

    void* reserved = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_READWRITE);
    void* reserved2 = VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_READWRITE);

    printf("%p\n%p\n", reserved, reserved2);

    void* p = VirtualAlloc(reserved, size + 1, MEM_COMMIT, PAGE_READWRITE);

    DWORD error = GetLastError();
    char* error_str;
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                  NULL, error, 0, error_str, 0, NULL);
    printf("%s\n", error_str);

    printf("allocations done\n"
           "reserved/commited pointers:\n%p\n%p\n", reserved, p);

    char* s = p;
    int i;
    for (i = 0; i < len; i++) {
        s[i] = string[i];
    }
    s[i] = '\0';

    printf("string: %s\n", s);

    printf("Done");
    return 0;
}