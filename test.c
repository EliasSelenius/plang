
#include <stdio.h>

typedef struct String {
    unsigned int length;
    char c_str[];
} String;

#define true 1
#define false 0

int main() {

    int b = false * true ? false : true;
    printf("%d\n", b);

    return 0;
}