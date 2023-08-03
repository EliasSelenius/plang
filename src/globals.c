
static Token* tokens; // darray
static u32 token_index = 0;

static Identifier type_name_int8 = 0;
static Identifier type_name_uint8 = 0;
static Identifier type_name_int16 = 0;
static Identifier type_name_uint16 = 0;
static Identifier type_name_int32 = 0;
static Identifier type_name_uint32 = 0;
static Identifier type_name_int64 = 0;
static Identifier type_name_uint64 = 0;
static Identifier type_name_float32 = 0;
static Identifier type_name_float64 = 0;
static Identifier type_name_char = 0;
static Identifier type_name_void = 0;


static Identifier builtin_string_print = 0;
static Identifier builtin_string_main = 0;

static void initTypenames() {
    type_name_int8    = register_string(spFrom("int8"));
    type_name_uint8   = register_string(spFrom("uint8"));
    type_name_int16   = register_string(spFrom("int16"));
    type_name_uint16  = register_string(spFrom("uint16"));
    type_name_int32   = register_string(spFrom("int32"));
    type_name_uint32  = register_string(spFrom("uint32"));
    type_name_int64   = register_string(spFrom("int64"));
    type_name_uint64  = register_string(spFrom("uint64"));
    type_name_float32 = register_string(spFrom("float32"));
    type_name_float64 = register_string(spFrom("float64"));
    type_name_char    = register_string(spFrom("char"));
    type_name_void    = register_string(spFrom("void"));

    builtin_string_print = register_string(spFrom("print"));
    builtin_string_main = register_string(spFrom("main"));
}

/*

    input -> parser -> unit
    units will contain syntax tree and arena so it can be freed independently from other units
    aswell as lists of unresolved symbols in the syntax tree

    units -> binder -> codebase
    binder will set correct references to variables and types in syntax tree (resolving symbols)
    then it will construct a Codebase object containing lists of pointers to all the things
    this process may be redone several times

    codebase -> validator
    typecheking

*/

typedef struct Unit {
    Arena arena;

} Unit;

Unit* current_unit;

Codebase bind_units(Unit* units) {
    // resolves unresolved symbols

    // constructs Codebase that contains lists of all procs/types/globals

    // must be possible to rebind codebase, because units may change
}

typedef struct Parser {
    File* src_files;
    u32 current_file_index;
    Scope* scope;

    Procedure* procedure; // current procedure that is being parsed

    // The local declarations of the currently parsed procedure
    Statement** stack;

    Statement** local_types;

    Type** unresolved_types; // list

    Token* tokens; // list
    u32 token_index;
} Parser;

static Parser parser;

static inline File* get_current_file() { return &parser.src_files[parser.current_file_index]; }
static inline File* get_file(u32 index) { return &parser.src_files[index]; }

static Identifier getNameOfStatement(Statement* sta) {
    switch (sta->statementType) {
        case Statement_FixedArray:
        case Statement_Declaration: return ((Declaration*)sta)->name;
        case Statement_Constant: return ((Declaration*)sta)->name;
        case Statement_Argument: return ((ProcArg*)sta)->name;
        case Statement_For: return ((ForStatement*)sta)->index_name;
        case Statement_Procedure: return ((Procedure*)sta)->name;

        case Statement_Struct: return ((Struct*)sta)->name;
        case Statement_Enum: return ((Enum*)sta)->name;
        case Statement_Typedef: return ((Typedef*)sta)->name;

        default: return 0;
    }
}


static void declare_local_type(Statement* sta) {
    list_add(parser.local_types, sta);
}

static Statement* get_local_type(Identifier name) {
    i32 len = (i32)list_length(parser.local_types);
    for (i32 i = len-1; i >= 0; i--) {
        Statement* sta = parser.local_types[i];
        if (name == getNameOfStatement(sta)) return sta;
    }

    return null;
}


static Statement* stack_get(Identifier name) {
    if (!parser.stack) return null;

    i32 len = (i32)list_length(parser.stack);
    for (i32 i = len-1; i >= 0; i--) {
        Statement* sta = parser.stack[i];
        if (name == getNameOfStatement(sta)) return sta;
    }

    return null;
}


static void stack_declare(Statement* sta) {

    // TODO: already declared error

    list_add(parser.stack, sta);
}

static void stack_pop(u32 num) {
    list_head(parser.stack)->length -= num;
}