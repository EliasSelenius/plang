
/*

    llvm-symbolizer.exe --use-native-pdb-reader --print-source-context-lines=20 -e ./bin/plang.exe

    TODO list:
        - localy defined structs
        *- clasic for loop: for uint32 i = 0, i < 10, i++ { }
        - for int loop: for 12 { }
        - loop block: loop { } == while (true) { }
        - nested multi-line comments
        - disallow void as variable type in declaration
        *- make assignments be expressions
        - contextual inclusion (the with keyword on struct fields)
        - member-like functions (the with keyword on function arguments)
        *- enums
        - explicitly sized enums e.g (enum Type : uint32 {})?
        - unions
        *- fixed sized arrays as struct fields
        - local consts
        - multi-declare e.g: int i, j = 1, k;
        - let output exe be named the name of the directory
        - it keyword
        - labeled break and continue e.g (for i in 0..count { if a break i; else continue i; }) use the index/item name as the for label
        - global variable runtime expression assignment
        - localy defined type aliases
        - operator overload
        - capturing locals for localy defined procedures
        - defer statement
        - print structs
        - named arguments e.g foo(arg_name: "daw")
        - single expression body

    InProgress:
        *- file name in errors
        *- line numbers in validation errors
        - modules or namespaces ?


    DONE list:
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

void addFile(char* filename, char* extension) {
    File file;
    u32 name_size = strlen(filename) + 1;
    file.filename = malloc(name_size);
    strcpy_s(file.filename, name_size, filename);

    list_add(Files, file);
}

int main(int argc, char* argv[]) {

    if (argc == 1) {
        printf("Insufficent arguments.\n");
        return 0;
    }

    // TODO: use higher default capacity here, to minimize the amount of reallocs
    tokens = list_create(Token);
    Files = list_create(File);
    codebase_init(&g_Codebase);


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

    { // tokenize
        foreach (file, Files) {
            u32 contents_size;
            char* contents = fileread(file->filename, &contents_size);
            if (!contents) continue;

            lex(contents);
            if (numberOfErrors) {
                printf("There were %d errors during tokenizing.\n", numberOfErrors);
                exit(0); // TODO: handle this properly
            }

            free(contents);
        }
    }

    if (false) { // print string table

        u32 len = list_length(g_Codebase.string_table.byteoffsets);
        for (u32 i = 0; i < len; i++) {
            char* s = get_string_byindex(i);
            printf("%d: %s\n", i, s);
        }

        printf("Printed %d strings\n", len);
    }

    parse();
    if (numberOfErrors) {
        printf("There were %d errors during parsing.\n", numberOfErrors);
        exit(0);
    }

    printf("Validate...\n");
    validate();
    if (numberOfErrors) {
        printf("There were %d errors during validation.\n", numberOfErrors);
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