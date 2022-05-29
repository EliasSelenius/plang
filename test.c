
#include <stdio.h>

typedef struct String {
    unsigned int length;
    char c_str[];
} String;

#define true 1
#define false 0

typedef struct Test {
    int i;
    int i1;
    char c;
} Test;

int main() {

    Test tests[2];

    printf("size: %llu\n", sizeof(Test));
    printf("size: %llu\n", sizeof(tests));

    return 0;
}