
/*

    llvm-symbolizer.exe --use-native-pdb-reader --print-source-context-lines=20 -e ./bin/plang.exe

    TODO list:
        *- clasic for loop: for uint32 i = 0, i < 10, i++ { }
        - for int loop: for 12 { }
        - loop block: loop { } == while (true) { }
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

    InProgress:
        - print structs
        *- file name in errors
        *- line numbers in validation errors
        - modules or namespaces ?
        *- array literal
        *- struct literal
        - contextual inclusion (the with keyword on struct fields)


    DONE list:
        - global variable runtime expression assignment
        - it keyword
        *- enums
        - localy defined structs
        - localy defined type aliases
        - local consts
        - nesting funcptr types inside funcptrs arguments. void(void*(char*), uint) seems to produce a bug
        - localy defined procedures
        - allow underscores in number literals
        - "plang build" CLI. build all .pog files in directory.
        *- for in range loop: for i in 0..10 { }
        *- implicit casts of numeric types
        *- built-in print() function
        *- switch statements
        *- bitwise assignments
        *- transpile decimal literals
        *- parse hex literals
        - omit curl brackets in if/while etc. for single statement block
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

#include <stdio.h>
#include <stdlib.h>

#include "prelude.h"
#include "essh-string.h"

void addFile(char* filename, char* extension);

typedef struct Codebase {

    struct Procedure** procedures; // list
    struct Declaration** global_vars; // list
    struct Declaration** global_consts; // list
    struct Struct** structs; // list
    struct Enum** enums; // list
    struct Typedef** type_defs; // list

} Codebase;

Codebase parse();

int main(int argc, char* argv[]) {

    if (argc == 1) {
        printf("Insufficent arguments.\n");
        return 0;
    }

    init_parser();
    init_string_table();

    startPerf();

    u32 i = 1;

    if (argc >= 2) {
        if (cstrEquals(argv[1], "build")) {
            foreachFile(".pog", addFile);
            i = 2;
        }
        // else if (cstrEquals(argv[1], "eval")) {
        //     printf("Evaling...\n");
        //     tokens = list_create(Token);
        //     lex(argv[2]);
        //     Expression* expr = expectExpression();
        //     Value v = interpret_expression(expr);
        //     print_value(v);
        //     return 0;
        // }
    }

    while (i < argc) {
        char* arg = argv[i++];
        if (spanEquals(spFrom("cflags"), arg)) break;
        // printf("    %d. %s\n", i, arg);
        addFile(arg, null);
    }

    StringBuilder sb = sbCreate();
    sbAppend(&sb, "clang -g output.g.c -o output.exe ");
    while (i < argc) {
        sbAppend(&sb, argv[i++]);
        sbAppendChar(&sb, ' ');
    }


    Codebase cb = parse();


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