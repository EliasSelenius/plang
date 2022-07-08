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
        - function overloads
        - localy defined structs
        *- clasic for loop: for (int i = 0; i < 10; i++)
        - for in range loop: for (i in 0..10)
        - for int loop: for (12) { }
        - loop block: loop { } == while (true) { }
        - modules or namespaces ?
        - allow underscores in number literals
        *- switch statements
        - omit curl brackets in if/while etc. for single statement block
        - hash identifiers (Tok_Word tokens) for faster string equals and to free the file buffer
        - nested multi-line comments
        - disallow void as variable type in declaration
        *- pre/post increment/decrement
        - nesting funcptr types inside funcptrs arguments. void(void*(char*), uint) seems to produce a bug
        *- make assignments be expressions 

    InProgress:
        - line numbers in validation errors
        - localy defined functions


    DONE list:
        *- a way to do stack allocation of arrays
        - function pointers
        - declare function pointer type in local scope
        *- char literal
        *- hex literal
        *- type agnostic constants (const pi = 3.14)
        *- casting (my_var as uint)
        *- better number literal type inference: 10 => int, 10.0 => f32, 10d => f64 ?
        - make void* implicitly castable to any other same degree pointer 
        - return type inference
        *- global variables
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

TranslationUnit* g_Unit;

char* fileread(const char* filename, u32* strLength) {
    FILE* file;
    if ( fopen_s(&file, filename, "r") ) {
        printf("Could not read file: %s\n", filename);
        return null;
    }

    fseek(file, 0, SEEK_END);
    int bufferLength = ftell(file) + 1; // CRLF gurantees there is enough space, but when the file is not in CRLF we add one to get space for null termination
    rewind(file);

    char* res = calloc(bufferLength, 1);
    u64 len = fread(res, 1, bufferLength, file);
    res[len] = '\0';
    *strLength = (u32)len;

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

void addPrimitiveType(char* name) {
    PlangType newType;
    newType.kind = Typekind_Primitive;
    newType.name = spFrom(name);
    darrayAdd(g_Unit->types, newType);
}

// plang glfw.txt cflags -g -lglfw3dll.lib
int main(int argc, char* argv[]) {

    if (argc == 1) {
        printf("Insufficent arguments.\n");
        return 0;
    }

    // TODO: use higher default capacity here, to minimize the amount of reallocs 
    tokens = darrayCreate(Token);

    static char* TypekindNames[] = {
        [Typekind_Invalid]   = "invalid  ",
        [Typekind_Primitive] = "primitive",
        [Typekind_Struct]    = "struct   ",
        [Typekind_Enum]      = "enum     ",
        [Typekind_Alias]     = "alias    ",
        [Typekind_FuncPtr]   = "function ",
    };

    TranslationUnit unit;
    unit.functions = darrayCreate(PlangFunction);
    unit.functionDeclarations = darrayCreate(FuncDeclaration);
    unit.structs = darrayCreate(PlangStruct);
    unit.globalVariables = darrayCreate(VarDecl);
    unit.constants = darrayCreate(Constant);
    unit.types = darrayCreate(PlangType);
    unit.funcPtrTypes = dyCreate();
    g_Unit = &unit;

    { // add default types to typetable
        addPrimitiveType("void");
        addPrimitiveType("char");

        addPrimitiveType("sbyte");
        addPrimitiveType("byte");
        addPrimitiveType("short");
        addPrimitiveType("ushort");
        addPrimitiveType("int");
        addPrimitiveType("uint");
        addPrimitiveType("long");
        addPrimitiveType("ulong");

        addPrimitiveType("float");
        addPrimitiveType("double");
    }

    startPerf();

    u32 i = 1;
    while (i < argc) {
        char* arg = argv[i++];
        if (spanEquals(spFrom("cflags"), arg)) break;
        printf("    %d. %s\n", i, arg);
        addFile(arg);
    }

    
    StringBuilder sb = sbCreate();
    sbAppend(&sb, "clang output.g.c -o output.exe ");

    while (i < argc) {
        sbAppend(&sb, argv[i++]);
        sbAppendChar(&sb, ' ');
    }

    printf("Validate...\n");
    u32 numErrors = validate();
    /*{ // print type table
        u32 len = darrayLength(g_Unit->types);
        for (u32 i = 0; i < len; i++) {
            printf("    %d. %s : %.*s\n", i, TypekindNames[g_Unit->types[i].kind], g_Unit->types[i].name.length, g_Unit->types[i].name.start);
        }
    }*/
    if (numErrors) {
        printf("There were %d errors during validation.\nFix errors and try again.\n", numErrors);
        return 0;
    }

    printf("Transpile...\n");
    transpile();

    i64 t = endPerf();
    printf("Done in %lldus.\n", t);


    printf("Compile...\n");
    printf("%s\n", sb.content);
    int code = system(sb.content);
    if (code == 0) {
        system("output.exe");
    } else {
        printf("clang return code: %d\n", code);
    }


    return 0;
}