#include "hello.g.h"
#include "..\main.g.h"

void sayHello() {
    Gameobject* g = malloc(sizeof(Gameobject));
    printf("%s", "I was told to say \"Hello\" so... Hello.\n");
} 
void localsTest() {
    int* g1 = malloc(sizeof(int) * 100);
    int* g2 = malloc(sizeof(int) * 100);
    if (1) {
        bool b = 0;
        if (1) {
            b = 1;
            int i = 12;
        } 
        i = 100;
    } 
    b = 1;
} 
