
#include <stdio.h>

int main() {

    int a, b, c;

    c = 33;

    a = (b = c = 12) + 2;

    a + 2;

    printf("a, b, c = %d %d %d\n", a, b, c);

    return 0;
}