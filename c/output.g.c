#include <stdlib.h>
#include <stdio.h>
#define true 1
#define false 0
// Structs

// Forward declarations
void printInt(int i);
int main();

// Globals

// Implementations
void printInt(int i) {
    printf("Int: %d\n", i);
}
int main() {
    printf("Testing binary operations.\n", 0);
    int a = 1;
    int e = 3;
    int x = ((a + (2 * 3)) + 4);
    int y = (((1 * 2) + 3) + 4);
    int _y = ((1 * (2 + e)) + 4);
    int g = (9 / (3 + 4));
    printInt(g);
    int z = ((1 - (9 / (3 + 7))) * 5);
    printInt(z);
    return 0;
}
