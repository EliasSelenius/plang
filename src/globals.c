
static u32 appendStringToStringtable(StrSpan word);

static char* g_Filename;
static Token* tokens; // darray
static u32 token_index = 0;

static u32 type_name_int8 = 0;
static u32 type_name_uint8 = 0;
static u32 type_name_int16 = 0;
static u32 type_name_uint16 = 0;
static u32 type_name_int32 = 0;
static u32 type_name_uint32 = 0;
static u32 type_name_int64 = 0;
static u32 type_name_uint64 = 0;
static u32 type_name_float32 = 0;
static u32 type_name_float64 = 0;
static u32 type_name_char = 0;
static u32 type_name_void = 0;


static u32 builtin_print_name = 0;

static void initTypenames() {
    type_name_int8    = appendStringToStringtable(spFrom("int8"));
    type_name_uint8   = appendStringToStringtable(spFrom("uint8"));
    type_name_int16   = appendStringToStringtable(spFrom("int16"));
    type_name_uint16  = appendStringToStringtable(spFrom("uint16"));
    type_name_int32   = appendStringToStringtable(spFrom("int32"));
    type_name_uint32  = appendStringToStringtable(spFrom("uint32"));
    type_name_int64   = appendStringToStringtable(spFrom("int64"));
    type_name_uint64  = appendStringToStringtable(spFrom("uint64"));
    type_name_float32 = appendStringToStringtable(spFrom("float32"));
    type_name_float64 = appendStringToStringtable(spFrom("float64"));
    type_name_char    = appendStringToStringtable(spFrom("char"));
    type_name_void    = appendStringToStringtable(spFrom("void"));

    builtin_print_name = appendStringToStringtable(spFrom("print"));
}