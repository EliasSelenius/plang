

// TODO: might be a good idea to make the string table be a hashmap, every time we append a new string it must go through the entire table
struct {
    u32* byteoffsets; // list
    DynamicBuffer* data;

    // Identifier* identifiers; // list
    // Arena arena;

} string_table;


static inline char* get_string(Identifier id) { return (char*)(&string_table.data->bytes[id]); }
static inline char* get_string_byindex(u32 index) { return (char*)(&string_table.data->bytes[string_table.byteoffsets[index]]); }

static Identifier register_string(StrSpan word) {
    u32 len = list_length(string_table.byteoffsets);
    for (u32 i = 0; i < len; i++) {
        u32 byteOffset = string_table.byteoffsets[i];
        char* s = (char*)(&string_table.data->bytes[byteOffset]);
        if (spanEquals(word, s)) return byteOffset;
    }

    u32 byteOffset = dyReserve(&string_table.data, word.length + 1);
    u8* p = (&string_table.data->bytes[byteOffset]);
    for (u32 i = 0; i < word.length; i++) p[i] = word.start[i];
    p[word.length] = '\0';

    list_add(string_table.byteoffsets, byteOffset);
    return byteOffset;
}

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
static Identifier builtin_string_it = 0;
static Identifier builtin_string_it_index = 0;
static Identifier builtin_string_length = 0;
static Identifier builtin_string_capacity = 0;
static Identifier builtin_string_elements = 0;
static Identifier builtin_string_add = 0;
static Identifier builtin_string_static_init = 0;


void init_string_table() {

    // string_table.identifiers = list_create(Identifier);
    // string_table.arena = arena_create();

    string_table.byteoffsets = list_create(u32);
    string_table.data = dyCreate();
    register_string(spFrom("")); // empty string

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
    builtin_string_it = register_string(spFrom("it"));
    builtin_string_it_index = register_string(spFrom("it_index"));
    builtin_string_length = register_string(spFrom("length"));
    builtin_string_capacity = register_string(spFrom("capacity"));
    builtin_string_elements = register_string(spFrom("elements"));
    builtin_string_add = register_string(spFrom("add"));

    builtin_string_static_init = register_string(spFrom("__static_init"));
}


static Procedure builtin_print_proc = {0};

static Token* tokens; // darray
static u32 token_index = 0;


typedef struct Parser {
    File* src_files;
    u32 current_file_index;
    Scope* scope;

    Procedure* procedure; // current procedure that is being validated

    Statement** local_symbols; // all the things a VariableExpression might refer to
    Statement** local_types;

    VariableExpression** unresolved_variables; // list
    Type** unresolved_types; // list

} Parser;

static Parser parser;

void init_parser() {
    parser = (Parser) {0};
    parser.src_files = list_create(File);

    parser.local_symbols = list_create(Statement*);
    parser.local_types = list_create(Statement*);

    parser.unresolved_variables = list_create(VariableExpression*);
    parser.unresolved_types = list_create(Type*);

}

static inline File* get_current_file() { return &parser.src_files[parser.current_file_index]; }
static inline File* get_file(u32 index) { return &parser.src_files[index]; }

static Identifier getNameOfStatement(Statement* sta) {
    switch (sta->statementType) {
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
    // TODO: already declared error
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

static Statement* get_global_type(Identifier name, Codebase* codebase) {
    {
        foreach (item, codebase->structs) {
            if ((*item)->name == name) return (Statement*)(*item);
        }
    }

    {
        foreach (item, codebase->type_defs) {
            if ((*item)->name == name) return (Statement*)(*item);
        }
    }

    {
        foreach (item, codebase->enums) {
            if ((*item)->name == name) return (Statement*)(*item);
        }
    }

    return null;
}

static Statement* get_global_symbol(Identifier name, Unit* unit) {
    foreach (item, unit->top_level_statements) {
        Statement* sta = *item;
        if (name == getNameOfStatement(sta)) return sta;
    }

    return null;
}

static Statement* get_symbol(Identifier name) {


    i32 len = (i32)list_length(parser.local_symbols);
    for (i32 i = len-1; i >= 0; i--) {
        Statement* sta = parser.local_symbols[i];
        if (name == getNameOfStatement(sta)) return sta;
    }

    return null;
}

static void declare_symbol(Statement* sta) {

    // TODO: already declared error

    list_add(parser.local_symbols, sta);
}

static void stack_pop(u32 num) {
    list_head(parser.local_symbols)->length -= num;
}