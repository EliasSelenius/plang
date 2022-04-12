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
        *- boolean operators and comparisons 
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
        *- function call 
        *- function arguments

    InProgress:


    DONE list:
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

typedef struct PlangFile {
    char* path;
    
    Token* tokens; // darray

    PlangFunction* functions; // darray
    PlangStruct* structs; // darray

} PlangFile;


int main(int argc, char* argv[]) {

    for (u32 i = 0; i < argc; i++) {
        printf("    %d. %s\n", i, argv[i]);
    }

    u32 filesize;
    char* text = fileread("lexTest.txt", &filesize);

    printf("Tokenize...\n");
    lex(text);

    /*for (u32 i = 0; i < tokens_length; i++) {
        Token token = tokens[i];

        printf("Token %d: |%.*s|\n", token.type, token.value.length, token.value.start);
    }*/

    printf("Parse...\n");
    parse();
    
    printf("Validate...\n");
    validate();

    printf("Transpile...\n");
    transpile();

    printf("Compile...\n");
    system("clang output.g.c -o output.exe");

    return 0;
}