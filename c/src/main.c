#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "types.h"
#include "parser.h"
#include "lexer.h"
#include "darray.h"
#include "essh-string.h"


void transpile();


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

void filewrite(const char* filename, char* content) {
    FILE* file = fopen(filename, "w");

    if (file == NULL) {
        printf("Could not write to file: %s\n", filename);
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


void validateFunction(PlangFunction* func) {
    for (u32 i = 0; i < darrayLength(func->scope.statements); i++) {
        Statement* sta = &func->scope.statements[i];
        switch (sta->statementType) {
            case Statement_Declaration: {
                // TODO: is already declared?
                
                VarDecl* decl = (VarDecl*)sta->node;
                if (decl->mustInferType) {
                    if (decl->assignmentOrNull) {
                        decl->type = getExpressedType(decl->assignmentOrNull);
                    } else {
                        // TODO: error message: var must be assigned to to be infered
                    }
                }

            } break;
            case Statement_Assignment: {
                // TODO: is variable declared?
            } break;
            case Statement_If: {
                // TODO: recurse
            } break;
            case Statement_While: {
                // TODO: recurse
            } break;
        }
    }
}

void validate() {
    for (u32 i = 0; i < darrayLength(functions); i++) {
        validateFunction(&functions[i]);
    }
}


int main(int argc, char* argv[]) {



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
    
    printf("Transpile...\n");
    transpile();

    return 0;
}