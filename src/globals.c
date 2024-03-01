

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


static void init_string_table() {

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


static void init_typenode_for_proc(Procedure* proc); // TODO: put all forward-decl in same file

static Procedure* builtin_procedures;

static Procedure create_builtin_proc(char* name) {
    Procedure proc = {0};
    proc.name = register_string(spFrom(name));
    proc.base.statementType = Statement_Procedure;
    init_typenode_for_proc(&proc);
    return proc;
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
    Arena arena; // TODO: make use of this arena
    Statement** top_level_statements; // list
    VariableExpression** external_symbols; // list
    Type** external_types; // list
    Identifier* included_files; // list
} Unit;

typedef struct Parser {

    Token* tokens; // list
    u32 token_index;

    Unit* units; // list

    Unit* current_unit;
    char* current_file_name;

    Scope* scope;         // current scope being parsed
    Procedure* procedure; // current procedure that is being validated

    Statement** local_symbols; // all the things a VariableExpression might refer to
    Statement** local_types;

    VariableExpression** unresolved_variables; // list
    Type** unresolved_types; // list

    Error* errors; // list

    // these options are added for REPL
    bool allow_lonely_expressions;
    bool allow_omitting_semicolon;

    jmp_buf jump_location;

} Parser;

static inline Token peek_at(Parser* parser, i32 offset) { return parser->tokens[parser->token_index + offset]; }
static inline Token peek(Parser* parser) { return peek_at(parser, 0); }
static inline Token advance(Parser* parser) { return parser->tokens[parser->token_index++]; }




static void gen_errorv(Parser* parser, CodeLocation loc, const char* format, va_list args) {
    Error e = {0};
    e.location = loc;
    e.message = alloc_vprintf(format, args);
    list_add(parser->errors, e);
}

static void gen_error(Parser* parser, CodeLocation loc, const char* format, ...) {
    va_list args;
    va_start(args, format);
    gen_errorv(parser, loc, format, args);
    va_end(args);
}

static void print_errors(Parser* parser) {
    foreach (err, parser->errors) {
        printf("%s:%d:%d: error: %s\n", err->location.file_name, err->location.line, err->location.column, err->message);
    }
}

static void fatal_parse_error(Parser* parser, char* format, ...) {
    Token tok = peek(parser);
    CodeLocation loc = { .file_name = parser->current_file_name, .line = tok.line, .column = tok.column };

    va_list args;
    va_start(args, format);
    gen_errorv(parser, loc, format, args);
    va_end(args);

    longjmp(parser->jump_location, 1);
}

static void error_token(Parser* parser, char* format, ...) {
    Token tok = peek(parser);
    CodeLocation loc = { .file_name = parser->current_file_name, .line = tok.line, .column = tok.column };

    va_list args;
    va_start(args, format);
    gen_errorv(parser, loc, format, args);
    va_end(args);
}

static void error_node(Parser* parser, void* node, char* format, ...) {
    Node* n = (Node*)node;

    va_list args;
    va_start(args, format);
    gen_errorv(parser, n->loc, format, args);
    va_end(args);
}




static void reset_parser(Parser* parser) {
    list_clear(parser->unresolved_variables);
    parser->token_index = 0;
}

Parser* init_parser() {

    if (string_table.data == null) {
        init_string_table();

        builtin_procedures = list_create(Procedure);
        list_add(builtin_procedures, create_builtin_proc("print"));
        list_add(builtin_procedures, create_builtin_proc("add"));
    }

    Parser* parser = calloc(1, sizeof(Parser));

    parser->tokens = list_create(Token);
    parser->token_index = 0;

    parser->units = list_create(Unit);

    parser->local_symbols = list_create(Statement*);
    parser->local_types = list_create(Statement*);

    parser->unresolved_variables = list_create(VariableExpression*);
    parser->unresolved_types = list_create(Type*);

    parser->errors = list_create(Error);

    return parser;
}

void free_parser(Parser* parser) {
    // TODO: ...
}


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


static Statement* get_global_type(Identifier name, Codebase* codebase) {
    foreach (stru, codebase->structs)   if ((*stru)->name == name) return (Statement*)(*stru);
    foreach (tdef, codebase->type_defs) if ((*tdef)->name == name) return (Statement*)(*tdef);
    foreach (enom, codebase->enums)     if ((*enom)->name == name) return (Statement*)(*enom);

    return null;
}

static Statement* get_global_symbol_from_codebase(Identifier name, Codebase* cb) {
    foreach (prc, cb->procedures)    if ((*prc)->name == name) return (Statement*)(*prc);
    foreach (var, cb->global_vars)   if ((*var)->name == name) return (Statement*)(*var);
    foreach (con, cb->global_consts) if ((*con)->name == name) return (Statement*)(*con);

    return null;
}

static Statement* get_global_symbol(Identifier name, Unit* unit) {
    foreach (item, unit->top_level_statements) {
        Statement* sta = *item;
        if (name == getNameOfStatement(sta)) return sta;
    }

    return null;
}

static void declare_local_type(Parser* parser, Statement* sta) {
    // TODO: already declared error
    list_add(parser->local_types, sta);
}

static Statement* get_local_type(Parser* parser, Identifier name) {
    i32 len = (i32)list_length(parser->local_types);
    for (i32 i = len-1; i >= 0; i--) {
        Statement* sta = parser->local_types[i];
        if (name == getNameOfStatement(sta)) return sta;
    }

    return null;
}

static Statement* get_symbol(Parser* parser, Identifier name) {

    i32 len = (i32)list_length(parser->local_symbols);
    for (i32 i = len-1; i >= 0; i--) {
        Statement* sta = parser->local_symbols[i];
        if (name == getNameOfStatement(sta)) return sta;
    }

    u32 builtin_procs_len = list_length(builtin_procedures);
    for (u32 i = 0; i < builtin_procs_len; i++) {
        if (builtin_procedures[i].name == name) return (Statement*)(&builtin_procedures[i]);
    }

    return null;
}

static void declare_symbol(Parser* parser, Statement* sta) {

    // TODO: already declared error

    list_add(parser->local_symbols, sta);
}

static void stack_pop(Parser* parser, u32 num) {
    list_head(parser->local_symbols)->length -= num;
}