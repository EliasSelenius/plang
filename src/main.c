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
        - function pointers
        - function overloads
        - localy defined structs
        *- clasic for loop: for (int i = 0; i < 10; i++)
        - for in range loop: for i in 0..10
        - loop block: loop { } == while (true) { }
        - modules or namespaces ?
        *- better number literal type inference: 10 => int, 10.0 => f32, 10d => f64 ?
        - allow underscores in number literals
        *- switch statements
        - omit curl brackets in if/while etc. for single statement block
        - hash identifiers (Tok_Word tokens) for faster string equals and to free the file buffer
        - nested multi-line comments

    InProgress:
        - line numbers in validation errors
        *- global variables
        - localy defined functions
        - return type inference


    DONE list:
        *- unary not operator
        *- address-of operator *
        *- value-of operator @
        *- not equals operator
        *- boolean operators
        *- boolean comparisons 
        *- multiple files 
        *- struct self-reference
        *- restructure expression types
        - scopes
        - type missmatch in assignments and declarations
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
        return null;
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

void addFile(char* filename) {
    u32 filesize = 0;
    char* fileContent = fileread(filename, &filesize);
    if (!fileContent) return;

    printf("Tokenize...\n");
    u32 numErrors = lex(fileContent);
    if (numErrors) {
        printf("There were %d errors during tokenizing.\nFix errors and try again.\n", numErrors);
        exit(0); // TODO: handle this properly
    }

    printf("Parse...\n");
    numErrors = parse();
    if (numErrors) {
        printf("There were %d errors during parsing.\nFix errors and try again.\n", numErrors);
        exit(0);
    }

    // TODO: Tokens are refering to the fileContent, so we can't free yet, but we probably should
    // free(fileContent);

}

void startPerf();
i64 endPerf();

int main(int argc, char* argv[]) {

    if (argc == 1) {
        printf("Insufficent arguments.\n");
        return 0;
    }

    // TODO: use higher default capacity here, to minimize the amount of reallocs 
    tokens = darrayCreate(Token);

    startPerf();

    printf("Files:\n");
    for (u32 i = 1; i < argc; i++) {
        printf("    %d. %s\n", i, argv[i]);
        addFile(argv[i]);
    }

    printf("Validate...\n");
    u32 numErrors = validate();
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