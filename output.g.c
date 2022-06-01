#include <stdlib.h>
#define true 1
#define false 0
// Structs
typedef struct Entity Entity;
typedef struct Test Test;
typedef struct Person Person;
typedef struct Entity {
    char* name;
    int x;
    int y;
} Entity;
typedef struct Test {
    float x;
    float y;
    Test* test;
    void* ptr;
} Test;
typedef struct Person {
    char* name;
    int age;
} Person;

// Forward declarations
void println(char* str);
char* yo();
int main();
Entity* make_entity(char* name);
void yes(int i, char* str);
void sayFoo(int i, char* str);
int get(int my_number);
void sayHello(Person* p);
void printf(char* str, char* arg);

// Globals
char* g_Name = "Elias";

// Implementations
void println(char* str) {
    printf("%s\n", str);
}
char* yo() {
    return "Yo!";
}
int main() {
    println("    Hello, Plang!");
    printf("Hello, This is printf() from %s\n", "plang");
    {
        int i;
        int* ip = &i;
        int a = *ip;
        int b = !true;
        if ((!b && !false)) {
            println("unary not test");
        }
        if (!(b && true)) {
            println("Other unary test");
        }
    }
    int booleanValue = true;
    char* str = (false && true) ? "Yes" : booleanValue ? "Okay Then" : "No";
    println(str);
    int i = 0;
    while (((i < 10) && (i != (4 + 2)))) {
        println("Loop.");
        i += 1;
    }
    {
        int isSome = true;
        if ((isSome || false)) {
            println("YEES");
        } else {
            println("NOOO");
        }
    }
    Test test;
    test.ptr = 0;
    if (test.ptr) {
        println("There is a test");
    } else {
        println("No There is no test");
    }
    Person* p = malloc(sizeof(Person));
    p->name = g_Name;
    sayHello(p);
    return 0;
}
Entity* make_entity(char* name) {
    Entity* e = malloc(sizeof(Entity));
    e->name = name;
    e->x = 0;
    e->y = 0;
    return e;
}
void yes(int i, char* str) {
    char* s = "dwadwaddd";
}
void sayFoo(int i, char* str) {
    return;
}
int get(int my_number) {
    return my_number;
}
void sayHello(Person* p) {
    println("Hello, From Person.");
}
