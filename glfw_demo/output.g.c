#include <stdlib.h>
#define true 1
#define false 0

// types
typedef unsigned int uint;
typedef void f_void_float_float_float_float(float, float, float, float);
typedef void f_void_int(int);
typedef uint f_uint_uint(uint);
typedef void f_void_uint_int_charpp_intp(uint, int, char**, int*);
typedef void f_void_uint_uint(uint, uint);
typedef void f_void_uint(uint);
typedef uint f_uint();

// Structs

// Forward declarations
void loadGL();
int main();
void println(char* str);
char* fileread(char* filename);
int glfwInit();
void glfwTerminate();
void* glfwCreateWindow(int width, int height, char* title, void* monitor, void* share);
void glfwPollEvents();
void glfwDestroyWindow(void* window);
void glfwSwapBuffers(void* window);
int glfwWindowShouldClose(void* window);
void glfwMakeContextCurrent(void* window);
void* glfwGetProcAddress(char* name);
int fopen_s(void** stream, char* filename, char* mode);
int fclose(void* stream);
int fseek(void* stream, int offset, int origin);
int ftell(void* stream);
void rewind(void* stream);
int fread(void* buffer, int elementSize, int elementCount, void* stream);
void printf(char* format, char* arg1);
void* calloc(int count, int size);

// Globals
int GL_COLOR_BUFFER_BIT = 16384;
f_void_float_float_float_float* glClearColor;
f_void_int* glClear;
f_uint_uint* glCreateShader;
f_void_uint_int_charpp_intp* glShaderSource;
f_void_uint_uint* glAttachShader;
f_void_uint_uint* glDetachShader;
f_void_uint* glDeleteShader;
f_uint* glCreateProgram;
f_void_uint* glLinkProgram;

// Implementations
void loadGL() {
    glClearColor = glfwGetProcAddress("glClearColor");
    glClear = glfwGetProcAddress("glClear");
    glCreateShader = glfwGetProcAddress("glCreateShader");
    glShaderSource = glfwGetProcAddress("glShaderSource");
    glAttachShader = glfwGetProcAddress("glAttachShader");
    glDetachShader = glfwGetProcAddress("glDetachShader");
    glDeleteShader = glfwGetProcAddress("glDeleteShader");
    glCreateProgram = glfwGetProcAddress("glCreateProgram");
    glLinkProgram = glfwGetProcAddress("glLinkProgram");
}
int main() {
    char* content = fileread("io.txt");
    println(content);
    if (!glfwInit()) {
        return -1;
    }
    int width = 1600;
    int height = 900;
    void* window = glfwCreateWindow(width, height, "Window", 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    loadGL();
    uint shader = glCreateProgram();
    glClearColor(0.0, 1.0, 1.0, 1.0);
    while (!glfwWindowShouldClose(window)) {
        glClear(GL_COLOR_BUFFER_BIT);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
void println(char* str) {
    printf("%s\n", str);
}
char* fileread(char* filename) {
    void* stream;
    if (fopen_s(&stream, filename, "r")) {
        println("Could not open file");
        return 0;
    }
    fseek(stream, 0, 2);
    int len = ftell(stream);
    rewind(stream);
    char* res = calloc((len + 1), 1);
    fread(res, 1, len, stream);
    fclose(stream);
    return res;
}
