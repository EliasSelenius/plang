#include <stdlib.h>
#include <stdio.h>
#define true 1
#define false 0
// Structs
typedef struct vec3 vec3;
typedef struct Transform Transform;
typedef struct Entity Entity;
typedef struct vec3 {
    float x;
    float y;
    float z;
} vec3;
typedef struct Transform {
    vec3 pos;
} Transform;
typedef struct Entity {
    Transform trans;
} Entity;

// Forward declarations
Entity* createEntity(int x, int z);
int get_Number(int i);
int main();

// Globals
Entity entity;

// Implementations
Entity* createEntity(int x, int z) {
    Entity* e = malloc(sizeof(Entity));
    return e;
}
int get_Number(int i) {
    return (i * 2);
}
int main() {
    printf("Testing deref.%d\n", 0);
    int n = get_Number(2);
    printf("number = %d\n", n);
    Entity e;
    Entity ep = e;
    entity = e;
    float x = entity.trans.pos.x;
    Transform t = createEntity(0, 12)->trans;
    vec3 f = t.pos;
    vec3* vpp = malloc(sizeof(vec3));
    vec3 h = *(vpp + 2);
    return 0;
}
