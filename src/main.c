
/*

    -O0 -g -fsanitize=address -fno-omit-frame-pointer

    llvm-symbolizer.exe --use-native-pdb-reader --print-source-context-lines=20 -e ./bin/plang.exe

    TODO list:
        - nested multi-line comments
        - disallow void as variable type in declaration
        - make assignments be expressions
        - member-like functions (the with keyword on function arguments)
        - explicitly sized enums e.g (enum Type : uint32 {})?
        - unions
        *- fixed sized arrays as struct fields
        - multi-declare e.g: int i, j = 1, k;
        - let output exe be named the name of the directory
        - labeled break and continue e.g (for i in 0..count { if a break i; else continue i; }) use the index/item name as the for label
        - operator overload
        - capturing locals for localy defined procedures
        - defer statement
        - named arguments e.g foo(arg_name: "daw")
        - single expression body
        - field initializers
        - redundant cast warning

    InProgress:
        - print structs
        *- file name in errors
        *- array literal
        *- struct literal
        - contextual inclusion (the with keyword on struct fields)

upcoming todos:
 - make sure the parser_parse_source() never exits

 - improved arenas (look at raddbg)
 - lists in arenas?

 - include paths relative to file


 - NodeRef in Datatype
     - remove init_typenode_for_proc
 - for loop parsing
 - get operator string (for Invalid binary expression error)


*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "prelude.h"
#include "essh-string.h"
#include "../platform/platform.h"

#include "public.h"

static Parser* g_Parser;


void add_file(FileInfo info, void* user_data) {
    // if (string_ends_with(info.name, ".pog")) parser_add_input_file(g_Parser, info.name);
}


int main(int argc, char* argv[]) {

    if (argc == 1) {
        printf("Insufficent arguments.\n");
        return 0;
    }

    g_Parser = init_parser();

    startPerf();

    u32 i = 1;

    bool evaling = false;

    if (argc >= 2) {
        if (cstrEquals(argv[1], "build")) {
            printf("NOT IMPLEMENTED: build option\n");
            exit(1);
            enumerate_files(".", add_file, null, true);
            i = 2;
        }
        else if (cstrEquals(argv[1], "eval")) {
            i = 2;
            evaling = true;
        }
    }

    while (i < argc) {
        char* arg = argv[i++];
        if (spanEquals(spFrom("cflags"), arg)) break;
        // printf("    %d. %s\n", i, arg);
        parser_parse_file(g_Parser, arg);
    }

    StringBuilder sb = sbCreate();
    sbAppend(&sb, "clang -g output.g.c -o output.exe ");
    while (i < argc) {
        sbAppend(&sb, argv[i++]);
        sbAppendChar(&sb, ' ');
    }


    Codebase cb = parse(g_Parser);

    if (evaling) {

        REPL repl;
        repl_init(&repl, &cb);

        char input[256];
        while (fgets(input, sizeof(input), stdin)) {
            repl_input(input, &repl);
        }
        return 0;
    }

    // if (false) { // hash test
    //     foreach (index, string_table.byteoffsets) {
    //         char* str = &string_table.data->bytes[*index];

    //         printf("%s -> %llu\n", str, string_hash(str));
    //     }

    //     return 1;
    // }


    printf("Transpile...\n");
    transpile(&cb);

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