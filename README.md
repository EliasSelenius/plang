# plang

Plang (or pog?) is a programming language. It can be best decribed as a pragmatic language. This project consists of a parser and a transpiler.

The reason for plangs existence is because I found myself discontent with every other language. Eventually I started using C which is by far the most pragmatic language. However there are problems with C aswell, so since I was so unhappy with everything, I just made my own language, that way, I can finally program in happiness.

Assume plang looks like C, except plang has:

- No preprocessor,
- Some minuscule syntax changes,
- Order-Independent declarations,
- Type inference,
- Function overloads,
- Introspection,

# Some syntax samples


``` c#
// this sample uses glfw

int32 main() {

    // option to omit both parenthesis and curly-brackets here
    if !glfwInit() return -1;

    // infer type with the 'let' keyword
    let window = glfwCreateWindow(1600, 900, "My Window", null, null);
    if !window {
        glfwTerminate();
        return -1;
    }

    glfwMakeContextCurrent(window);
    load_opengl(glfwGetProcAddress);

    glClearColor(0.4, 1.0, 0.1, 1.0);

    let error = glGetError();
    if error print("OpenGL error. %\n", error);

    while !glfwWindowShouldClose(window) {
        glClear(GL_COLOR_BUFFER_BIT);

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
```