#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "types.h"
#include "parser.h"
#include "lexer.h"
#include "validator.h"
#include "darray.h"
#include "essh-string.h"



/*


    TODO list:
        *- boolean operators
        *- boolean comparisons 
        - function pointers
        - function overloads
        - return type inference
        - subfunctions
        *- clasic for loop: for (int i = 0; i < 10; i++)
        - for in range loop: for i in 0..10
        - loop block: loop { } == while (true) { }
        - modules or namespaces ?
        *- better number literal type inference: 10 => int, 10.0 => f64, 10f => f32 ?
        - allow underscores in number literals
        *- global variables
        - scopes
        *- switch statements
        - line numbers in validation errors

    InProgress:


    DONE list:
        *- declare statement (declare function signature without implementation)
        *- function call 
        *- function arguments
        *- transpile forward declarations
        - ternary operator
        *- deref structs/pointers: foo.bar
        *- indexing: p[2]
        *- else if
        *- assignments
        *- while
        - return statement
        *- break, continue
        - null literal

*/

void transpile();


char* fileread(const char* filename, u32* strLength) {
    FILE* file;
    if ( fopen_s(&file, filename, "r") ) {
        printf("Could not read file: %s\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *strLength = ftell(file);
    rewind(file);

    char* res = calloc(*strLength + 1, sizeof(char));
    fread(res, 1, *strLength, file);

    fclose(file);
    
    return res;
}

void filewrite(const char* filename, char* content) {
    FILE* file;
    if ( fopen_s(&file, filename, "w") ) {
        printf("Could not write to file: %s\n", filename);
        return;
    }

    fprintf(file, "%s", content);

    fclose(file);
}

// typedef struct PlangFile {
//     char* path;
    
//     Token* tokens; // darray

//     PlangFunction* functions; // darray
//     PlangStruct* structs; // darray

// } PlangFile;


void startPerf();
i64 endPerf();

int main(int argc, char* argv[]) {

    for (u32 i = 0; i < argc; i++) {
        printf("    %d. %s\n", i, argv[i]);
    }

    startPerf();

    u32 filesize;
    char* text = fileread("lexTest.txt", &filesize);

    printf("Tokenize...\n");
    u32 numErrors = lex(text);
    if (numErrors) {
        printf("There were %d errors during tokenizing.\nFix errors and try again.\n", numErrors);
        return 0;
    }

    printf("Parse...\n");
    numErrors = parse();
    if (numErrors) {
        printf("There were %d errors during parsing.\nFix errors and try again.\n", numErrors);
        return 0;
    }

    printf("Validate...\n");
    numErrors = validate();

    if (numErrors) {
        printf("There were %d errors during validation.\nFix errors and try again.\n", numErrors);
        return 0;
    }

    printf("Transpile...\n");
    transpile();

    i64 t = endPerf();
    printf("Done in %lldus.\n", t);


    printf("Compile...\n");
    int code = system("clang output.g.c -o output.exe");
    if (code == 0) {
        system("output.exe");
    } else {
        printf("clang return code: %d\n", code);
    }


    return 0;
}