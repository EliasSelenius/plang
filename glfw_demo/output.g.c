#include <stdlib.h>
#include <stdio.h>
#define true 1
#define false 0
// Structs

// Forward declarations
char* test();
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

// Implementations
char* test() {
    return "Hello Window!";
}
int main() {
    if (!glfwInit()) {
        return -1;
    }
    void* func = test;
    char* title = test();
    int width = 1600;
    int height = 900;
    void* window = glfwCreateWindow(width, height, title, 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    gladLoadGL(glfwGetProcAddress);
    while (!glfwWindowShouldClose(window)) {
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
