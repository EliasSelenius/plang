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
int main();

// Globals

// Implementations
int main() {
    printf("Testing deref.\n");
    Entity e;
    Entity ep = e;
    float x = e.trans.pos.x;
    return 0;
}
