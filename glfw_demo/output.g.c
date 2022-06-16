#define true 1
#define false 0

// types
typedef unsigned int uint;
typedef unsigned char byte;
typedef unsigned short ushort;
typedef void f_void_float_float_float_float(float, float, float, float);
typedef void f_void_uint(uint);
typedef uint f_uint();
typedef uint f_uint_uint(uint);
typedef void f_void_uint_int_charpp_intp(uint, int, char**, int*);
typedef void f_void_uint_uint(uint, uint);
typedef void f_void_uint_uint_intp(uint, uint, int*);
typedef void f_void_uint_uint_intp_charp(uint, uint, int*, char*);
typedef void f_void_uint_int_int(uint, int, int);
typedef void f_void();
typedef f_void* GLFWglproc;
typedef f_void* GLFWvkproc;
typedef struct GLFWmonitor GLFWmonitor;
typedef struct GLFWwindow GLFWwindow;
typedef struct GLFWcursor GLFWcursor;
typedef void f_void_int_charp(int, char*);
typedef f_void_int_charp* GLFWerrorfun;
typedef void f_void_GLFWwindowp_int_int(GLFWwindow*, int, int);
typedef f_void_GLFWwindowp_int_int* GLFWwindowposfun;
typedef f_void_GLFWwindowp_int_int* GLFWwindowsizefun;
typedef void f_void_GLFWwindowp(GLFWwindow*);
typedef f_void_GLFWwindowp* GLFWwindowclosefun;
typedef f_void_GLFWwindowp* GLFWwindowrefreshfun;
typedef void f_void_GLFWwindowp_int(GLFWwindow*, int);
typedef f_void_GLFWwindowp_int* GLFWwindowfocusfun;
typedef f_void_GLFWwindowp_int* GLFWwindowiconifyfun;
typedef f_void_GLFWwindowp_int* GLFWwindowmaximizefun;
typedef f_void_GLFWwindowp_int_int* GLFWframebuffersizefun;
typedef void f_void_GLFWwindowp_float_float(GLFWwindow*, float, float);
typedef f_void_GLFWwindowp_float_float* GLFWwindowcontentscalefun;
typedef void f_void_GLFWwindowp_int_int_int(GLFWwindow*, int, int, int);
typedef f_void_GLFWwindowp_int_int_int* GLFWmousebuttonfun;
typedef void f_void_GLFWwindowp_double_double(GLFWwindow*, double, double);
typedef f_void_GLFWwindowp_double_double* GLFWcursorposfun;
typedef f_void_GLFWwindowp_int* GLFWcursorenterfun;
typedef f_void_GLFWwindowp_double_double* GLFWscrollfun;
typedef void f_void_GLFWwindowp_int_int_int_int(GLFWwindow*, int, int, int, int);
typedef f_void_GLFWwindowp_int_int_int_int* GLFWkeyfun;
typedef void f_void_GLFWwindowp_uint(GLFWwindow*, uint);
typedef f_void_GLFWwindowp_uint* GLFWcharfun;
typedef void f_void_GLFWwindowp_uint_int(GLFWwindow*, uint, int);
typedef f_void_GLFWwindowp_uint_int* GLFWcharmodsfun;
typedef void f_void_GLFWwindowp_int_charpp(GLFWwindow*, int, char**);
typedef f_void_GLFWwindowp_int_charpp* GLFWdropfun;
typedef void f_void_GLFWmonitorp_int(GLFWmonitor*, int);
typedef f_void_GLFWmonitorp_int* GLFWmonitorfun;
typedef void f_void_int_int(int, int);
typedef f_void_int_int* GLFWjoystickfun;
typedef struct GLFWvidmode GLFWvidmode;
typedef struct GLFWgammaramp GLFWgammaramp;
typedef struct GLFWimage GLFWimage;
typedef struct GLFWgamepadstate GLFWgamepadstate;

// Structs
typedef struct GLFWvidmode {
    int width;
    int height;
    int redBits;
    int greenBits;
    int blueBits;
    int refreshRate;
} GLFWvidmode;
typedef struct GLFWgammaramp {
    ushort* red;
    ushort* green;
    ushort* blue;
    uint size;
} GLFWgammaramp;
typedef struct GLFWimage {
    int width;
    int height;
    byte* pixels;
} GLFWimage;
typedef struct GLFWgamepadstate {
    int removeThisField;
} GLFWgamepadstate;

// Forward declarations
void loadGL();
uint makeShader(uint program, uint _type, char* code);
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
int glfwInit();
void glfwTerminate();
void glfwInitHint(int hint, int value);
void glfwGetVersion(int* major, int* minor, int* rev);
char* glfwGetVersionString();
int glfwGetError(char** description);
GLFWerrorfun glfwSetErrorCallback(GLFWerrorfun callback);
GLFWmonitor** glfwGetMonitors(int* count);
GLFWmonitor* glfwGetPrimaryMonitor();
void glfwGetMonitorPos(GLFWmonitor* monitor, int* xpos, int* ypos);
void glfwGetMonitorWorkarea(GLFWmonitor* monitor, int* xpos, int* ypos, int* width, int* height);
void glfwGetMonitorPhysicalSize(GLFWmonitor* monitor, int* widthMM, int* heightMM);
void glfwGetMonitorContentScale(GLFWmonitor* monitor, float* xscale, float* yscale);
char* glfwGetMonitorName(GLFWmonitor* monitor);
void glfwSetMonitorUserPointer(GLFWmonitor* monitor, void* pointer);
void* glfwGetMonitorUserPointer(GLFWmonitor* monitor);
GLFWmonitorfun glfwSetMonitorCallback(GLFWmonitorfun callback);
GLFWvidmode* glfwGetVideoModes(GLFWmonitor* monitor, int* count);
GLFWvidmode* glfwGetVideoMode(GLFWmonitor* monitor);
void glfwSetGamma(GLFWmonitor* monitor, float gamma);
GLFWgammaramp* glfwGetGammaRamp(GLFWmonitor* monitor);
void glfwSetGammaRamp(GLFWmonitor* monitor, GLFWgammaramp* ramp);
void glfwDefaultWindowHints();
void glfwWindowHint(int hint, int value);
void glfwWindowHintString(int hint, char* value);
GLFWwindow* glfwCreateWindow(int width, int height, char* title, GLFWmonitor* monitor, GLFWwindow* share);
void glfwDestroyWindow(GLFWwindow* window);
int glfwWindowShouldClose(GLFWwindow* window);
void glfwSetWindowShouldClose(GLFWwindow* window, int value);
void glfwSetWindowTitle(GLFWwindow* window, char* title);
void glfwSetWindowIcon(GLFWwindow* window, int count, GLFWimage* images);
void glfwGetWindowPos(GLFWwindow* window, int* xpos, int* ypos);
void glfwSetWindowPos(GLFWwindow* window, int xpos, int ypos);
void glfwGetWindowSize(GLFWwindow* window, int* width, int* height);
void glfwSetWindowSizeLimits(GLFWwindow* window, int minwidth, int minheight, int maxwidth, int maxheight);
void glfwSetWindowAspectRatio(GLFWwindow* window, int numer, int denom);
void glfwSetWindowSize(GLFWwindow* window, int width, int height);
void glfwGetFramebufferSize(GLFWwindow* window, int* width, int* height);
void glfwGetWindowFrameSize(GLFWwindow* window, int* left, int* top, int* right, int* bottom);
void glfwGetWindowContentScale(GLFWwindow* window, float* xscale, float* yscale);
float glfwGetWindowOpacity(GLFWwindow* window);
void glfwSetWindowOpacity(GLFWwindow* window, float opacity);
void glfwIconifyWindow(GLFWwindow* window);
void glfwRestoreWindow(GLFWwindow* window);
void glfwMaximizeWindow(GLFWwindow* window);
void glfwShowWindow(GLFWwindow* window);
void glfwHideWindow(GLFWwindow* window);
void glfwFocusWindow(GLFWwindow* window);
void glfwRequestWindowAttention(GLFWwindow* window);
GLFWmonitor* glfwGetWindowMonitor(GLFWwindow* window);
void glfwSetWindowMonitor(GLFWwindow* window, GLFWmonitor* monitor, int xpos, int ypos, int width, int height, int refreshRate);
int glfwGetWindowAttrib(GLFWwindow* window, int attrib);
void glfwSetWindowAttrib(GLFWwindow* window, int attrib, int value);
void glfwSetWindowUserPointer(GLFWwindow* window, void* pointer);
void* glfwGetWindowUserPointer(GLFWwindow* window);
GLFWwindowposfun glfwSetWindowPosCallback(GLFWwindow* window, GLFWwindowposfun callback);
GLFWwindowsizefun glfwSetWindowSizeCallback(GLFWwindow* window, GLFWwindowsizefun callback);
GLFWwindowclosefun glfwSetWindowCloseCallback(GLFWwindow* window, GLFWwindowclosefun callback);
GLFWwindowrefreshfun glfwSetWindowRefreshCallback(GLFWwindow* window, GLFWwindowrefreshfun callback);
GLFWwindowfocusfun glfwSetWindowFocusCallback(GLFWwindow* window, GLFWwindowfocusfun callback);
GLFWwindowiconifyfun glfwSetWindowIconifyCallback(GLFWwindow* window, GLFWwindowiconifyfun callback);
GLFWwindowmaximizefun glfwSetWindowMaximizeCallback(GLFWwindow* window, GLFWwindowmaximizefun callback);
GLFWframebuffersizefun glfwSetFramebufferSizeCallback(GLFWwindow* window, GLFWframebuffersizefun callback);
GLFWwindowcontentscalefun glfwSetWindowContentScaleCallback(GLFWwindow* window, GLFWwindowcontentscalefun callback);
void glfwPollEvents();
void glfwWaitEvents();
void glfwWaitEventsTimeout(double timeout);
void glfwPostEmptyEvent();
int glfwGetInputMode(GLFWwindow* window, int mode);
void glfwSetInputMode(GLFWwindow* window, int mode, int value);
int glfwRawMouseMotionSupported();
char* glfwGetKeyName(int key, int scancode);
int glfwGetKeyScancode(int key);
int glfwGetKey(GLFWwindow* window, int key);
int glfwGetMouseButton(GLFWwindow* window, int button);
void glfwGetCursorPos(GLFWwindow* window, double* xpos, double* ypos);
void glfwSetCursorPos(GLFWwindow* window, double xpos, double ypos);
GLFWcursor* glfwCreateCursor(GLFWimage* image, int xhot, int yhot);
GLFWcursor* glfwCreateStandardCursor(int shape);
void glfwDestroyCursor(GLFWcursor* cursor);
void glfwSetCursor(GLFWwindow* window, GLFWcursor* cursor);
GLFWkeyfun glfwSetKeyCallback(GLFWwindow* window, GLFWkeyfun callback);
GLFWcharfun glfwSetCharCallback(GLFWwindow* window, GLFWcharfun callback);
GLFWcharmodsfun glfwSetCharModsCallback(GLFWwindow* window, GLFWcharmodsfun callback);
GLFWmousebuttonfun glfwSetMouseButtonCallback(GLFWwindow* window, GLFWmousebuttonfun callback);
GLFWcursorposfun glfwSetCursorPosCallback(GLFWwindow* window, GLFWcursorposfun callback);
GLFWcursorenterfun glfwSetCursorEnterCallback(GLFWwindow* window, GLFWcursorenterfun callback);
GLFWscrollfun glfwSetScrollCallback(GLFWwindow* window, GLFWscrollfun callback);
GLFWdropfun glfwSetDropCallback(GLFWwindow* window, GLFWdropfun callback);
int glfwJoystickPresent(int jid);
float* glfwGetJoystickAxes(int jid, int* count);
char* glfwGetJoystickButtons(int jid, int* count);
char* glfwGetJoystickHats(int jid, int* count);
char* glfwGetJoystickName(int jid);
char* glfwGetJoystickGUID(int jid);
void glfwSetJoystickUserPointer(int jid, void* pointer);
void* glfwGetJoystickUserPointer(int jid);
int glfwJoystickIsGamepad(int jid);
GLFWjoystickfun glfwSetJoystickCallback(GLFWjoystickfun callback);
int glfwUpdateGamepadMappings(char* string);
char* glfwGetGamepadName(int jid);
int glfwGetGamepadState(int jid, GLFWgamepadstate* state);
void glfwSetClipboardString(GLFWwindow* window, char* string);
char* glfwGetClipboardString(GLFWwindow* window);
double glfwGetTime();
void glfwSetTime(double time);
unsigned long long glfwGetTimerValue();
unsigned long long glfwGetTimerFrequency();
void glfwMakeContextCurrent(GLFWwindow* window);
GLFWwindow* glfwGetCurrentContext();
void glfwSwapBuffers(GLFWwindow* window);
void glfwSwapInterval(int interval);
int glfwExtensionSupported(char* extension);
GLFWglproc glfwGetProcAddress(char* procname);
int glfwVulkanSupported();
char** glfwGetRequiredInstanceExtensions(uint* count);

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
uint makeShader(uint program, uint _type, char* code) {
    uint s = glCreateShader(_type);
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
