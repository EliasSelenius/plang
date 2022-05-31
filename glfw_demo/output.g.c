#include <stdlib.h>
#include <stdio.h>
#define true 1
#define false 0
// Structs

// Forward declarations
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
int gladLoadGL(void* load);

// Globals
int GL_COLOR_BUFFER_BIT = 16384;

// Implementations
void getString(char** out_string) {
    *out_string = "pli da ŝanĝo";
}
int main() {
    if (!glfwInit()) {
        return -1;
    }
    void* constructTitleFunc = getString;
    char* title = "dwa";
    ((void (*)(char**))constructTitleFunc)(&title);
    int width = 1600;
    int height = 900;
    void* window = glfwCreateWindow(width, height, title, 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    void* glClearColor = glfwGetProcAddress("glClearColor");
    void* glClear = glfwGetProcAddress("glClear");
    ((void (*)(float, float, float, float))glClearColor)(1.0, 1.0, 0.0, 1.0);
    while (!glfwWindowShouldClose(window)) {
        ((void (*)(int))glClear)(GL_COLOR_BUFFER_BIT);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
