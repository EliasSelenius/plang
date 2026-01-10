

// TODO: might be a good idea to make the string table be a hashmap, every time we append a new string it must go through the entire table
struct {
    u32* byteoffsets; // list
    DynamicBuffer* data;

    // TODO: remove dynamic array completely in favor of an arena

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

#define builtin_strings(X) \
    X(int8) \
    X(uint8) \
    X(int16) \
    X(uint16) \
    X(int32) \
    X(uint32) \
    X(int64) \
    X(uint64) \
    X(float32) \
    X(float64) \
    X(char) \
    X(void) \
    X(string) \
    X(print) \
    X(main) \
    X(it) \
    X(it_index) \
    X(length) \
    X(capacity) \
    X(elements) \
    X(add) \
    X(static_init) \
    X(chars) \
    X(data) \


#define decl_strings(str) static Identifier builtin_string_##str = 0;
builtin_strings(decl_strings)
#undef decl_strings

static void init_string_table() {
    string_table.byteoffsets = list_create(u32);
    string_table.data = dyCreate();
    register_string(spFrom("")); // empty string

    #define init_strings(str) builtin_string_##str = register_string(spFrom(#str));
    builtin_strings(init_strings)
    #undef init_strings
}


static void init_typenode_for_proc(Procedure* proc); // TODO: put all forward-decl in same file

static Procedure* builtin_procedures;

static Procedure create_builtin_proc(char* name) {
    Procedure proc = {0};
    proc.name = register_string(spFrom(name));
    proc.kind = Node_Procedure;
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
    Arena* arena;
    NodeRef* top_level_nodes; // list
    VariableExpression** external_symbols; // list
    Type** external_types; // list
    Identifier* included_files; // list
} Unit;

typedef struct Parser {
    Codebase* codebase;

    Token* tokens; // list
    u32 token_index;

    Unit* units; // list

    Unit* current_unit;
    char* current_file_name;

    Declaration* multi_declaration;

    Scope* scope;         // current scope being parsed
    Procedure* procedure; // current procedure that is being validated

    NodeRef* local_symbols; // all the things a VariableExpression might refer to
    NodeRef* local_types;

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

static CodeLocation get_code_location_here(Parser* parser) {
    Token tok = peek(parser);
    return (CodeLocation) { .file_name = parser->current_file_name, .line = tok.line, .column = tok.column };
}

static NodeRef alloc_node(Parser* parser, Nodekind kind) {
    NodeRef ref = (NodeRef)arena_alloc(parser->current_unit->arena, node_struct_sizes[kind]);
    ref.node->kind = kind;
    ref.node->loc = get_code_location_here(parser);
    return ref;
}

static void print_errors(Parser* parser) {
    foreach (err, parser->errors) {
        printf("%s:%d:%d: error: %s\n",
            err->location.file_name,
            err->location.line,
            err->location.column,
            err->message);
    }
}

static void gen_errorv(Parser* parser, CodeLocation loc, const char* format, va_list args) {
    Error e = {0};
    e.location = loc;
    e.message = alloc_vprintf(format, args);
    list_add(parser->errors, e);
}

#define gen_error_m(parser, loc) do {\
    va_list args; va_start(args, format);\
    gen_errorv(parser, loc, format, args);\
    va_end(args);\
    } while (0)

static void gen_error(Parser* parser, CodeLocation loc, const char* format, ...) { gen_error_m(parser, loc); }
static void error_token(Parser* parser, char* format, ...) { gen_error_m(parser, get_code_location_here(parser)); }
static void error_node(Parser* parser, NodeRef ref, char* format, ...) { gen_error_m(parser, ref.node->loc); }
_Noreturn static void fatal_parse_error(Parser* parser, char* format, ...) {
    gen_error_m(parser, get_code_location_here(parser));
    longjmp(parser->jump_location, 1);
}

#undef gen_error_m




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

    parser->token_index = 0;
    list_init(parser->tokens);
    list_init(parser->units);
    list_init(parser->local_symbols);
    list_init(parser->local_types);
    list_init(parser->unresolved_variables);
    list_init(parser->unresolved_types);
    list_init(parser->errors);

    return parser;
}

void free_parser(Parser* parser) {
    // TODO: ...
}


static Identifier getNameOfStatement(NodeRef ref) {
    switch (ref.node->kind) {
        case Node_Declaration: return ref.Declaration->name;
        case Node_Constant:    return ref.Constant->name;
        case Node_Argument:    return ref.Argument->name;
        case Node_ForStmt:     return ref.ForStmt->index_name;
        case Node_Procedure:   return ref.Procedure->name;
        case Node_Struct:      return ref.Struct->name;
        case Node_Enum:        return ref.Enum->name;
        case Node_Typedef:     return ref.Typedef->name;

        default: return 0;
    }
}


static NodeRef get_global_symbol_from_codebase(Identifier name, Codebase* cb) {
    foreach (prc, cb->procedures)    if ((*prc)->name == name) return (NodeRef)(*prc);
    foreach (var, cb->global_vars)   if ((*var)->name == name) return (NodeRef)(*var);
    foreach (con, cb->global_consts) if ((*con)->name == name) return (NodeRef)(*con);

    return null_node;
}

static NodeRef get_global_symbol(Identifier name, Unit* unit) {
    foreach (item, unit->top_level_nodes) if (name == getNameOfStatement(*item)) return *item;
    return null_node;
}


// TODO: could this be consolidated with typeofStatement()
static Datatype get_datatype_from_statement(NodeRef ref) {
    if (node_is_null(ref)) return type_invalid;
    switch (ref.node->kind) {
        case Node_Struct:  return (Datatype) { .kind = Typekind_Struct, .data_ptr = ref.void_ptr };
        case Node_Enum:    return (Datatype) { .kind = Typekind_Enum, .data_ptr = ref.void_ptr };
        case Node_Typedef: return (Datatype) { .kind = Typekind_Typedef, .data_ptr = ref.void_ptr };
        default: return type_invalid;
    }
}

static Datatype get_local_type(Parser* parser, Identifier name) {
    i32 len = (i32)list_length(parser->local_types);
    for (i32 i = len-1; i >= 0; i--) {
        NodeRef ref = parser->local_types[i];
        if (name == getNameOfStatement(ref)) return get_datatype_from_statement(ref);
    }
    return type_invalid;
}

static Datatype get_global_type(Identifier name, Codebase* codebase) {
    foreach (stru, codebase->structs)   if ((*stru)->name == name) return (Datatype) { .kind = Typekind_Struct, .data_ptr = *stru };
    foreach (tdef, codebase->type_defs) if ((*tdef)->name == name) return (Datatype) { .kind = Typekind_Typedef, .data_ptr = *tdef };
    foreach (enom, codebase->enums)     if ((*enom)->name == name) return (Datatype) { .kind = Typekind_Enum, .data_ptr = *enom };
    return type_invalid;
}


static void declare_local_type(Parser* parser, NodeRef ref) {
    // TODO: already declared error
    list_add(parser->local_types, ref);
}

static NodeRef get_symbol(Parser* parser, Identifier name) {
    i32 len = (i32)list_length(parser->local_symbols);
    for (i32 i = len-1; i >= 0; i--) {
        NodeRef ref = parser->local_symbols[i];
        if (name == getNameOfStatement(ref)) return ref;
    }

    u32 builtin_procs_len = list_length(builtin_procedures);
    for (u32 i = 0; i < builtin_procs_len; i++) {
        if (builtin_procedures[i].name == name) return (NodeRef)(&builtin_procedures[i]);
    }

    return null_node;
}

static void declare_symbol(Parser* parser, NodeRef ref) {
    // TODO: already declared error
    list_add(parser->local_symbols, ref);
}

static void stack_pop(Parser* parser, u32 num) {
    list_head(parser->local_symbols)->length -= num;
}

static u32 get_symbol_stack_length(Parser* parser) { return list_head(parser->local_symbols)->length; }
static void set_symbol_stack_length(Parser* parser, u32 value) { list_head(parser->local_symbols)->length = value; }
