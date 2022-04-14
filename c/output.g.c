#include <stdlib.h>
// Structs
typedef struct Entity {
    char* name;
    float x;
    float y;
} Entity;
typedef struct Test {
    float x;
    float y;
} Test;

// Forward declarations
int main();
Entity* make_entity(char* name);
void yes(int i, char* str);
void sayFoo(int i, char* str);
int get(int my_number);

// Implementations
int main() {
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
}
void sayFoo(int i, char* str) {
    return;
}
int get(int my_number) {
    return my_number;
}
