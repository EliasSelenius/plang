#define true 1
#define false 0

// types
typedef unsigned int uint;
typedef void f_void_float_float_float_float(float, float, float, float);
typedef void f_void_uint(uint);
typedef uint f_uint();
typedef uint f_uint_uint(uint);
typedef void f_void_uint_int_charpp_intp(uint, int, char**, int*);
typedef void f_void_uint_uint(uint, uint);
typedef void f_void_uint_uint_intp(uint, uint, int*);
typedef void f_void_uint_uint_intp_charp(uint, uint, int*, char*);
typedef void f_void_uint_int_int(uint, int, int);

// Structs

// Forward declarations
void loadGL();
uint makeShader(uint program, uint type, char* code);
uint createShader();
int main();
void println(char* str);
void printNum(uint num);
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
unsigned long long ftell(void* stream);
void rewind(void* stream);
unsigned long long fread(void* buffer, unsigned long long elementSize, unsigned long long elementCount, void* stream);
void printf(char* format, char* arg1, uint arg2);
void* calloc(unsigned long long count, unsigned long long size);
void* malloc(unsigned long long size);

// Globals
uint GL_COLOR_BUFFER_BIT = 16384;
uint GL_VERTEX_SHADER = 35633;
uint GL_FRAGMENT_SHADER = 35632;
uint GL_TRIANGLES = 4;
uint GL_LINK_STATUS = 35714;
f_void_float_float_float_float* glClearColor;
f_void_uint* glClear;
f_uint* glGetError;
f_uint_uint* glCreateShader;
f_void_uint_int_charpp_intp* glShaderSource;
f_void_uint_uint* glAttachShader;
f_void_uint_uint* glDetachShader;
f_void_uint* glDeleteShader;
f_uint* glCreateProgram;
f_void_uint* glLinkProgram;
f_void_uint* glUseProgram;
f_void_uint_uint_intp* glGetProgramiv;
f_void_uint_uint_intp_charp* glGetProgramInfoLog;
f_void_uint_int_int* glDrawArrays;
uint shaderInfoLogSize = 1024;
char* shaderInfoLog = 0;

// Implementations
void loadGL() {
    glClearColor = glfwGetProcAddress("glClearColor");
    glClear = glfwGetProcAddress("glClear");
    glGetError = glfwGetProcAddress("glGetError");
    glCreateShader = glfwGetProcAddress("glCreateShader");
    glShaderSource = glfwGetProcAddress("glShaderSource");
    glAttachShader = glfwGetProcAddress("glAttachShader");
    glDetachShader = glfwGetProcAddress("glDetachShader");
    glDeleteShader = glfwGetProcAddress("glDeleteShader");
    glCreateProgram = glfwGetProcAddress("glCreateProgram");
    glLinkProgram = glfwGetProcAddress("glLinkProgram");
    glUseProgram = glfwGetProcAddress("glUseProgram");
    glGetProgramiv = glfwGetProcAddress("glGetProgramiv");
    glGetProgramInfoLog = glfwGetProcAddress("glGetProgramInfoLog");
    glDrawArrays = glfwGetProcAddress("glDrawArrays");
    shaderInfoLog = malloc(shaderInfoLogSize);
}
uint makeShader(uint program, uint type, char* code) {
    uint s = glCreateShader(type);
    glShaderSource(s, 1, &code, 0);
    glAttachShader(program, s);
    return s;
}
uint createShader() {
    uint program = glCreateProgram();
    char* vertCode = fileread("shaders/vert.glsl");
    char* fragCode = fileread("shaders/frag.glsl");
    uint v = makeShader(program, GL_VERTEX_SHADER, vertCode);
    uint f = makeShader(program, GL_FRAGMENT_SHADER, fragCode);
    glLinkProgram(program);
    glDetachShader(program, v);
    glDeleteShader(v);
    glDetachShader(program, f);
    glDeleteShader(f);
    {
        int status;
        glGetProgramiv(program, GL_LINK_STATUS, &status);
        if ((status == 0)) {
            int len;
            glGetProgramInfoLog(program, shaderInfoLogSize, &len, shaderInfoLog);
            println(shaderInfoLog);
        }
    }
    return program;
}
int main() {
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
    uint shader = createShader();
    glUseProgram(shader);
    glClearColor(0.0, 1.0, 1.0, 1.0);
    uint error = glGetError();
    if (error) {
        printf("%s %d\n", "OpenGL error.", error);
    }
    while (!glfwWindowShouldClose(window)) {
        glClear(GL_COLOR_BUFFER_BIT);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    glfwDestroyWindow(window);
    glfwTerminate();
}
void println(char* str) {
    printf("%s\n", str, 0);
}
void printNum(uint num) {
    printf("%s%d", "", num);
}
char* fileread(char* filename) {
    void* stream;
    if (fopen_s(&stream, filename, "r")) {
        println("Could not open file");
        return 0;
    }
    fseek(stream, 0, 2);
    unsigned long long len = ftell(stream);
    rewind(stream);
    char* res = calloc((len + 1), 1);
    fread(res, 1, len, stream);
    fclose(stream);
    return res;
}
