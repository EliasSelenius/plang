#include "main.g.h"
#include "daw.g.h"

int main() {
    printf("%s", "Hello, World!\n");
    foo();
    daw();
    int x = 10;
    if (1) {
        x = 12;
        y = 4;
        char* g = "foo";
        if (0) {
            g = null;
            x = 4;
        } 
    } 
    return 0;
} 
void foo() {
    printf("%s", "FooBar\n");
} 
