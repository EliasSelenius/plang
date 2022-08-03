
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

typedef struct String {
    unsigned int length;
    char c_str[];
} String;

#define true 1
#define false 0

typedef struct Test {
    int i, j;
} Test;

int size = 0;


int main() {

    char* str = "";

    int len = strlen(str);

    printf("%d\n", len);

    return 0;
}