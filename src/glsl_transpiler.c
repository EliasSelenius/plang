
typedef struct CodeBuilder {
    StringBuilder sb;
    u32 tabing;
} CodeBuilder;

static void cb_append(CodeBuilder* cb, char* str) {
    sbAppend(&cb->sb, str);
}

static void cb_newline(CodeBuilder* cb) {
    cb_append(cb, "\n");
    u32 t = cb->tabing;
    while (t--) cb_append(cb, "    "); // TODO: hardcoded tab size
}

static void glsl_transpile(CodeBuilder* cb, Procedure* proc) {
}

static void glsl_transpile_statement(CodeBuilder* cb, Statement* sta) {

}