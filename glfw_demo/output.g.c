#include <stdlib.h>
#define true 1
#define false 0
// Structs
typedef struct Shader Shader;
typedef struct Shader {
    int id;
} Shader;

// Forward declarations
void loadGL();
int main();
int glfwInit();
void glfwTerminate();
void* glfwCreateWindow(int width, int height, char* title, void* monitor, void* share);
void glfwPollEvents();
void glfwDestroyWindow(void* window);
void glfwSwapBuffers(void* window);
int glfwWindowShouldClose(void* window);
void glfwMakeContextCurrent(void* window);
void* glfwGetProcAddress(char* name);

// Globals
int GL_COLOR_BUFFER_BIT = 16384;
void* glClearColor;
void* glClear;

// Implementations
void loadGL() {
    glClearColor = glfwGetProcAddress("glClearColor");
    glClear = glfwGetProcAddress("glClear");
}
int main() {
    if (!glfwInit()) {
        return -1;
    }
    int width = 1600;
    int height = 900;
    void* window = glfwCreateWindow(width, height, "title", 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    loadGL();
    ((void (*)(float, float, float, float))glClearColor)(0.0, 1.0, 0.0, 1.0);
    while (!glfwWindowShouldClose(window)) {
        ((void (*)(int))glClear)(GL_COLOR_BUFFER_BIT);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
