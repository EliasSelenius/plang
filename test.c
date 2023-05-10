
#include <stdio.h>

void my_proc(int i) {
    printf("%d\n", i);
}

int main() {



    printf("Hello, Mother!\n");

    typedef struct Test {
        int i;
    } Test;

    Test test;
    test.i = 1;

    printf("%d\n", test.i);

    // {
    //     // void(char*)(void(int), float) Hello[16]

    //     void (*(*Hello[16])(void (*)(int), float))(char*);

    //     void (*ret)(char*) = (*Hello)(my_proc, 3.14);

    //     ret("Test");
    // }

    // {
    //     void (*(*(*p1)(int))(int))(int);

    //     void (*(*p2)(int))(int) = p1(12);

    //     void (*p3)(int) = p2(24);

    //     p3(48);
    // }


    return 0;
}

void otherProc() {
}