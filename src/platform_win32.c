#include <windows.h>
#include <stdio.h> // printf

#include "prelude.h"
#include "essh-string.h"
#include "platform.h"


#define PathSize 1024
static u32 pathLen = 0;
static char path[PathSize];

static void append(char* txt) {
    u32 len = strlen(txt);

    if (pathLen + len >= PathSize) { printf("Max path length exceded\n"); exit(1); }

    memcpy(path + pathLen, txt, len);
    pathLen += len;
    path[pathLen] = '\0';
}

static void pushPath(char* file) {
    if (pathLen) path[pathLen++] = '/';
    append(file);
}

static void popPath() {
    u32 i = pathLen - 1;
    while (i > 0) {
        if (path[i] == '/') break;
        i--;
    }
    path[i] = '\0';
    pathLen = i;
}

static char* getExt(char* filename) {
    char* ext = null;
    while (*filename) {
        if (*filename == '.') ext = filename;
        filename++;
    }
    return ext;
}


void foreachFile(char* ext, void (*func)(char* filename, char* extension)) {

    HANDLE handle;
    WIN32_FIND_DATA data;

    pushPath("*");
    handle = FindFirstFile(path, &data);
    popPath();

    if (handle == INVALID_HANDLE_VALUE) {
        printf("Invalid Handle\n");
        return;
    }

    do {
        if (data.cFileName[0] == '.') continue;

        if (data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            pushPath(data.cFileName);
            foreachFile(ext, func);
            popPath();
        } else {
            char* filenameExt = getExt(data.cFileName);
            if (ext) if (!cstrEquals(ext, filenameExt)) continue;

            pushPath(data.cFileName);
            func(path, filenameExt);
            popPath();
        }


    } while (FindNextFile(handle, &data));


    FindClose(handle);
}


// -------Timer---------------------------------------

static LARGE_INTEGER start, freq;
void startPerf() {
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&start);
}
i64 endPerf() {
    LARGE_INTEGER end;
    QueryPerformanceCounter(&end);

    LARGE_INTEGER elapsed;
    elapsed.QuadPart = end.QuadPart - start.QuadPart;

    elapsed.QuadPart *= 1000000;
    elapsed.QuadPart /= freq.QuadPart;

    return elapsed.QuadPart;
}



// ---------Virtual-Memory------------------------------


void* vmem_reserve(u32 size) { return VirtualAlloc(null, size, MEM_RESERVE, PAGE_NOACCESS); }
void vmem_release(void* address) { VirtualFree(address, 0, MEM_RELEASE); }
void vmem_commit(void* address, u32 size) { VirtualAlloc(address, size, MEM_COMMIT, PAGE_READWRITE); }
void vmem_decommit(void* address, u32 size) { VirtualFree(address, size, MEM_DECOMMIT); }