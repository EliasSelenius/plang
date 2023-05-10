
static File* Files; // list

static char* g_Filename;
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

typedef struct LocalDecl {
    Identifier name;
    Reference ref;
} LocalDecl;

typedef struct Parser {
    File* src_files;
    File* current_file;
    Scope* scope;

    // The local declarations of the currently parsed procedure
    LocalDecl* stack;

    // Declaration** stack;


    Reference last_reference; // used by validateProcCall to get the Procedure that might be overloaded

    Type** unresolved_types; // list

    Token* tokens;
    u32 token_index;
} Parser;

static Parser parser;

static Reference stack_get(Identifier name) {
    if (!parser.stack) return reference_invalid;

    i32 len = (i32)list_length(parser.stack);
    for (i32 i = len-1; i >= 0; i--) {
        LocalDecl local = parser.stack[i];
        if (local.name == name) return local.ref;
    }

    return reference_invalid;
}

static void stack_declare(Identifier name, RefType type, void* data) {

    Reference ref = stack_get(name);
    if (ref.reftype != RefType_Invalid) {
        // TODO: already declared error
    }

    LocalDecl local = (LocalDecl) { .name = name, .ref = { .reftype = type, .data = data }};
    list_add(parser.stack, local);
}

static void stack_pop(u32 num) {
    list_head(parser.stack)->length -= num;
}