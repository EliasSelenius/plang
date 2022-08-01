
#include <windows.h>
#include <stdio.h>

#include "types.h"
#include "essh-string.h"

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

char* fileread(const char* filename, u32* strLength) {
    FILE* file;
    if ( fopen_s(&file, filename, "r") ) {
        printf("Could not read file: %s\n", filename);
        return null;
    }

    fseek(file, 0, SEEK_END);
    int bufferLength = ftell(file) + 1; // CRLF gurantees there is enough space, but when the file is not in CRLF we add one to get space for null termination
    rewind(file);

    char* res = calloc(bufferLength, 1);
    u64 len = fread(res, 1, bufferLength, file);
    res[len] = '\0';
    *strLength = (u32)len;

    fclose(file);

    return res;
}

void filewrite(const char* filename, char* content) {
    FILE* file;
    if ( fopen_s(&file, filename, "w") ) {
        printf("Could not write to file: %s\n", filename);
        return;
    }

    fprintf(file, "%s", content);

    fclose(file);
}