

static u32 numberOfErrors = 0;


// static void error(char* format, ...) {
//     printf("Error Ln%d: ", tokens[token_index].line);

//     va_list args;
//     va_start(args, format);
//     vprintf(format, args);
//     va_end(args);

//     printf("\n");
//     numberOfErrors++;
// }

// validator

static void error_v(u32 line, char* filename, char* format, va_list args) {
    printf("%s:%d: error: ", filename, line);
    vprintf(format, args);
    printf("\n");
    numberOfErrors++;
}

static void error(u32 line, char* filename, char* format, ...) {
    va_list args;
    va_start(args, format);
    error_v(line, filename, format, args);
    va_end(args);
}

static void error_line(u32 line, char* format, ...) {
    va_list args;
    va_start(args, format);
    error_v(line, "In unknown file", format, args)
    va_end(args);
}



static void error_statement(char* format, ...);