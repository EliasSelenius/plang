// Structs
typedef struct Entity {
    char* name;
    float x;
    float y;
} Entity;
typedef struct Foo {
    int number;
} Foo;

// Forward declarations
int main();
Entity* makeEntity(char* name);
void yes(int i, char* str);
Foo sayFoo(int i, char* str);

// Implementations
int main() {
    Foo f = sayFoo(12, "d");
    Entity fs;
    fs.name = "dkwa";
    return 0;
}
Entity* makeEntity(char* name) {
    Entity* e = malloc(sizeof(Entity));
    e->name = name;
    e->x = 0;
    e->y = 0;
    return e;
}
void yes(int i, char* str) {
}
Foo sayFoo(int i, char* str) {
}
