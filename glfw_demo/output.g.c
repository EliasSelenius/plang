#include <stdlib.h>
#define true 1
#define false 0

// types
typedef unsigned int uint;
typedef void (*f_void_float_float_float_float)(float, float, float, float);
typedef void (*f_void_int)(int);
typedef void (*f_void)();
typedef struct Shader Shader;
typedef Shader* (*f_Shaderp)();
typedef struct Test Test;
typedef int* (*f_intp_int_Test)(int, Test);
typedef char* (*f_charp)();

// Structs
typedef struct Shader {
    uint id;
    f_void func;
} Shader;
typedef struct Test {
    float c;
} Test;

// Forward declarations
void loadGL();
Shader* getShader();
char* invoke(f_charp func);
void sayHello();
char* getTitle();
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
void printf(char* format, char* arg1);

// Globals
int GL_COLOR_BUFFER_BIT = 16384;
f_void_float_float_float_float glClearColor;
f_void_int glClear;
f_Shaderp shaderRetrivalFunc;
f_intp_int_Test ptrOfInt;

// Implementations
void loadGL() {
}
Shader* getShader() {
    return malloc(sizeof(Shader));
}
char* invoke(f_charp func) {
    return func();
}
void sayHello() {
    printf("%s\n", "Hello");
}
char* getTitle() {
    return "The Title Of The Window";
}
int main() {
    if (!glfwInit()) {
        return -1;
    }
    f_charp titleFunc = getTitle;
    shaderRetrivalFunc = getShader;
    f_Shaderp srf = shaderRetrivalFunc;
    Shader* shader = srf();
    shader->func = sayHello;
    f_void sfunc = shader->func;
    sfunc();
    int width = 1600;
    int height = 900;
    void* window = glfwCreateWindow(width, height, invoke(titleFunc), 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    loadGL();
    while (!glfwWindowShouldClose(window)) {
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
