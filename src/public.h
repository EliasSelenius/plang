

typedef struct Codebase {

    struct Procedure** procedures; // list
    struct Declaration** global_vars; // list
    struct Constant** global_consts; // list
    struct Struct** structs; // list
    struct Enum** enums; // list
    struct Typedef** type_defs; // list

} Codebase;

typedef struct REPL {
    Codebase* codebase; // can be null
    struct ReplScope* context;
    struct Parser* parser;
} REPL;

typedef struct Parser Parser;

typedef struct CodeLocation {
    char* file_name;
    u32 line, column;
} CodeLocation;

typedef struct Error {
    CodeLocation location;
    char* message;
} Error;

Parser* init_parser();
void parser_parse_source(Parser* parser, char* source);
void parser_parse_file(Parser* parser, char* file_name);

Codebase parse(Parser* parser);
void transpile(Codebase* codebase);

void repl_init(REPL* repl, Codebase* cb);
u64 repl_input(char* code, REPL* repl);
