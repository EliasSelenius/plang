#include "main.g.h"
#include "daw.g.h"

int main() {
    printf("%s", "Hello, World!\n");
    foo();
    daw();
    int x = 10;
    if (1) {
        x = 12;
        char* g = "foo";
        if (0) {
            g = ((void*)0);
            x = 4;
        } 
    } 
    printf("%s", "\n");
    printf("%s", "Now is the end of the application, good bye!\n");
    return 0;
} 
void foo() {
    printf("%s", "FooBar\n");
} 
