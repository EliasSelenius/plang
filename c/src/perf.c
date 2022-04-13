#include <Windows.h>
#include <stdio.h>
#include "types.h"

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