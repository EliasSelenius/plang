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

    llvm-symbolizer.exe --use-native-pdb-reader --print-source-context-lines=20 -e ./bin/plang.exe

    TODO list:
        - localy defined structs
        *- clasic for loop: for (int i = 0; i < 10; i++)
        - for in range loop: for (i in 0..10)
        - for int loop: for (12) { }
        - loop block: loop { } == while (true) { }
        - modules or namespaces ?
        - allow underscores in number literals
        *- switch statements
        - omit curl brackets in if/while etc. for single statement block
        - nested multi-line comments
        - disallow void as variable type in declaration
        - nesting funcptr types inside funcptrs arguments. void(void*(char*), uint) seems to produce a bug
        *- make assignments be expressions 
        - "plang build" CLI. build all .pog files in directory. let output exe be named the name of the directory
        - contextual inclusion (the with keyword on struct fields)
        - member-like functions (the with keyword on function arguments)
        *- file name in errors

    InProgress:
        *- line numbers in validation errors
        - localy defined functions


    DONE list:
        - construct list of all string identifiers during tokenizing, to allow for faster string equals and to free the file buffer.
        - function overloads
        *- goto statement and labels
        *- unary minus operation
        *- modulus operator
        *- bitwise operators
        *- sizeof()
        *- pre/post increment/decrement
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
void startPerf();
i64 endPerf();
void foreachFile(char* ext, void (*func)(char* filename, char* extension));
char* fileread(const char* filename, u32* strLength);
void filewrite(const char* filename, char* content);

TranslationUnit* g_Unit;

void addFile(char* filename, char* extension) {
    printf("Adding %s\n", filename);

    u32 filesize = 0;
    char* fileContent = fileread(filename, &filesize);
    if (!fileContent) return;

    // printf("Tokenize...\n");
    u32 numErrors = lex(fileContent);
    if (numErrors) {
        printf("There were %d errors during tokenizing.\nFix errors and try again.\n", numErrors);
        exit(0); // TODO: handle this properly
    }

    free(fileContent);

    // printf("Parse...\n");
    numErrors = parse();
    if (numErrors) {
        printf("There were %d errors during parsing.\nFix errors and try again.\n", numErrors);
        exit(0);
    }

}



u32 appendStringToStringtable(StrSpan word) {
    u32 len = darrayLength(g_Unit->stringTableByteOffsets);
    for (u32 i = 0; i < len; i++) {
        u32 byteOffset = g_Unit->stringTableByteOffsets[i];
        char* s = (char*)(&g_Unit->stringTable->bytes[byteOffset]);
        if (spanEquals(word, s)) return byteOffset;
    }

    u32 byteOffset = dyReserve(&g_Unit->stringTable, word.length + 1);
    u8* p = (&g_Unit->stringTable->bytes[byteOffset]);
    for (u32 i = 0; i < word.length; i++) p[i] = word.start[i];
    p[word.length] = '\0';

    darrayAdd(g_Unit->stringTableByteOffsets, byteOffset);
    return byteOffset;
}

void test(u64 number) {


}


int main(int argc, char* argv[]) {

    if (argc == 1) {
        printf("Insufficent arguments.\n");
        return 0;
    }

    // TODO: use higher default capacity here, to minimize the amount of reallocs
    tokens = darrayCreate(Token);


    TranslationUnit unit;
    unit.functions = darrayCreate(PlangFunction);
    unit.functionDeclarations = darrayCreate(FuncDeclaration);
    unit.structs = darrayCreate(PlangStruct);
    unit.globalVariables = darrayCreate(VarDecl);
    unit.constants = darrayCreate(Constant);
    unit.funcPtrTypes = dyCreate();
    unit.aliases = darrayCreate(AliasType);
    unit.opaqueTypes = darrayCreate(Identifier);
    unit.stringTable = dyCreate();
    unit.stringTableByteOffsets = darrayCreate(u32);
    g_Unit = &unit;

    initTypenames();

    startPerf();

    u32 i = 1;

    if (argc >= 2) {
        if (cstrEquals(argv[1], "build")) {
            foreachFile(".pog", addFile);
            i = 2;
            goto skipfiles;
        }
    }

    while (i < argc) {
        char* arg = argv[i++];
        if (spanEquals(spFrom("cflags"), arg)) break;
        printf("    %d. %s\n", i, arg);
        addFile(arg, null);
    }

    skipfiles:

    StringBuilder sb = sbCreate();
    sbAppend(&sb, "clang output.g.c -o output.exe ");

    while (i < argc) {
        sbAppend(&sb, argv[i++]);
        sbAppendChar(&sb, ' ');
    }


    if (false) { // print string table

        u32 len = darrayLength(g_Unit->stringTableByteOffsets);
        for (u32 i = 0; i < len; i++) {
            u8* s = &g_Unit->stringTable->bytes[g_Unit->stringTableByteOffsets[i]];
            printf("%s\n", s);
        }

        printf("Printed %d strings\n", len);
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
    printf("%s\n", sb.content);
    int code = system(sb.content);
    if (code == 0) {
        system("output.exe");
    } else {
        printf("clang return code: %d\n", code);
    }


    return 0;
}