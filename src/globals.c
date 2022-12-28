
typedef struct File {
    char* filename;
    Namespace* namespace;
} File;

static File* Files; // list
static File* context; // currently parsed file
static Namespace* current_namespace; // currently validated namespace


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


static Identifier builtin_print_name = 0;

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

    builtin_print_name = register_string(spFrom("print"));
}