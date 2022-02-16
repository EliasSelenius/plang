#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "types.h"
#include "parser.h"
#include "lexer.h"


char* fileread(const char* filename, u32* strLength) {
    FILE* file = fopen(filename, "r");

    if (file == NULL) {
        printf("Could not read file: %s\n", filename);
        perror("fileread() error");
    }

    fseek(file, 0, SEEK_END);
    *strLength = ftell(file);
    rewind(file);


    char* res = calloc(*strLength + 1, sizeof(char));
    fread(res, 1, *strLength, file);

    //printf("file:%s has length: %d, and strlen = %llu\n", filename, *strLength, strlen(res));

    fclose(file);

    return res;
}

Expression* gExpressions[16];
u32 exprIndex = 0;

int main(int argc, char* argv[]) {

    // char text[] = "void main() }  Hello\n    12\nWorld!\n    DAw\nvoid main() {\n    let x = 12;\n    x += 10;\n    if (x == 12 || x <= 12) return;}";

    u32 filesize;
    char* text = fileread("lexTest.txt", &filesize);
    // char* text = fileread("src/main.c", &filesize);


    printf("Start lexing\n");
    lex(text);
    printf("End lexing\n");

    for (u32 i = 0; i < tokens_length; i++) {
        Token token = tokens[i];

        printf("Token %d: |%.*s|\n", token.type, token.value.length, token.value.start);
    }

    printf("No more tokens\n");

    printf("Start Parsing...\n");

    parse();

    printf("End Parsing...\n");

    PlangFunction* funcs = &functions;
    for (u32 i = 0; i < func_count; i++) {
        printf("func: %.*s\n", functions[i].name.length, functions[i].name.start);
    }

    PlangStruct* strus = &structs;
    for (u32 i = 0; i < struct_count; i++) {
        printf("struct: %.*s\n", structs[i].name.length, structs[i].name.start);
    }

    return 0;
}