#pragma once
#include "main.g.h"
#include "other.g.h"

int main();
void foo(int a, int b, char* str);
char* bar();
typedef struct Self {
    struct Self* self;
} Self;
typedef struct Transform {
    vec2 pos;
    vec2 scale;
} Transform;
typedef struct Gameobject {
    Transform transform;
    struct Gameobject* parent;
    int child_count;
    struct Gameobject** children;
} Gameobject;
char* getMessage();
