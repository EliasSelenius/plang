#include <stdlib.h>
#include <stdio.h>
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
int main();
Entity* make_entity(char* name);
void yes(int i, char* str);
void sayFoo(int i, char* str);
int get(int my_number);
void sayHello(Person* p);

// Globals
char* g_Name = "Elias";

// Implementations
int main() {
    printf("    Hello, Plang!\n");
    Test test;
    test.ptr = 0;
    if (test.ptr) {
        printf("There is a test\n");
    } else {
        printf("No There is no test\n");
    }
    Person* p = malloc(sizeof(Person));
    p->name = g_Name;
    sayHello(p);
    printf("Persons name is: ");
    printf(p->name);
    printf("\n");
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
    printf("Hello, From Person.\n");
}
