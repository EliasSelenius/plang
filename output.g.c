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

// Globals

// Implementations
int main() {
    if (!glfwInit()) {
        return -1;
    }
    int width = 1600;
    int height = 900;
    void* window = glfwCreateWindow(width, height, "GLFW Window in Plang", 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    while (!glfwWindowShouldClose(window)) {
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
