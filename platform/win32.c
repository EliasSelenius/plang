#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "../src/prelude.h"
#include "../src/essh-string.h"

#include "platform.h"

#include <Windows.h>

char* path_get_filename(char* path) {
    char* res = path;
    char* buffer = res;
    while (*buffer) {
        if (*buffer == '/') res = buffer + 1;
        buffer++;
    }
    return res;
}

#define make_u64(high, low) ((u64)low | ((u64)high << 32))
static u64 make_u64_from_FILETIME(FILETIME t) { return make_u64(t.dwHighDateTime, t.dwLowDateTime); }

static void enumerate_files_internal(StringBuilder* sb, enumerate_files_callback callback, void* user_data, bool recursive) {
    HANDLE handle;
    WIN32_FIND_DATA data;

    sbAppend(sb, "/*"); // match all files with '*' wildcard
    handle = FindFirstFile(sb->content, &data);
    // NOTE: we are not setting the zero terminator here
    sb->length -= 2; // length of "/*"

    if (handle == INVALID_HANDLE_VALUE) {
        u32 error = GetLastError();
        if (error == ERROR_FILE_NOT_FOUND) return;
        if (error == ERROR_PATH_NOT_FOUND) return;

        printf("ERROR: %u in enumerate_files\n", error);
        return;
    }


    do {
        if (data.cFileName[0] == '.') continue; // skip dot files like .git .vscode . .. etc
        // if (cstrEquals(".", data.cFileName) || cstrEquals("..", data.cFileName)) continue;

        u32 len = sb->length;
        sbAppend(sb, "/");
        sbAppend(sb, data.cFileName);

        if (recursive && (data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ) {
            enumerate_files_internal(sb, callback, user_data, recursive);
        } else {
            FileInfo file = {0};
            file.path = sb->content;
            file.name = data.cFileName;
            file.size = make_u64(data.nFileSizeHigh, data.nFileSizeLow);
            file.creation_time = make_u64_from_FILETIME(data.ftCreationTime);
            file.last_access_time = make_u64_from_FILETIME(data.ftLastAccessTime);
            file.last_write_time = make_u64_from_FILETIME(data.ftLastWriteTime);

            callback(file, user_data);
        }

        sb->length = len;

    } while (FindNextFile(handle, &data));

    FindClose(handle);
}

void enumerate_files(char* path, enumerate_files_callback callback, void* user_data, bool recursive) {
    StringBuilder* sb = temp_builder();
    sbAppend(sb, path);
    enumerate_files_internal(sb, callback, user_data, recursive);
}



static void print_time(u64 t) {
    FILETIME ft = {0};
    ft.dwLowDateTime = t & 0xffffffff;
    ft.dwHighDateTime = t >> 32;

    FILETIME lft = {0};
    SYSTEMTIME systime = {0};

    FileTimeToLocalFileTime(&ft, &lft);
    FileTimeToSystemTime(&lft, &systime);


    printf("%hu/%hu/%hu %hu:%hu", systime.wDay, systime.wMonth, systime.wYear, systime.wHour, systime.wMinute);
}

void print_file(FileInfo info, void* user_data) {
    printf("file: \"%s\" (%llu bytes) ", info.path, info.size);
    print_time(info.last_write_time);
    printf("\n");
}


// ---------Virtual-Memory------------------------------


void* vmem_reserve(u64 size) { return VirtualAlloc(null, size, MEM_RESERVE, PAGE_NOACCESS); }
void vmem_release(void* address) { VirtualFree(address, 0, MEM_RELEASE); }
void vmem_commit(void* address, u64 size) { VirtualAlloc(address, size, MEM_COMMIT, PAGE_READWRITE); }
void vmem_decommit(void* address, u64 size) { VirtualFree(address, size, MEM_DECOMMIT); }


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