
#include <stdlib.h> // included for malloc
#include <string.h> // included for strlen()
#include <stdio.h>  // printf
#include <stdarg.h>
#include <math.h>
#include <assert.h>


#include "prelude.h"
#include "io.c"
#include "platform.h"

#include "essh-string.h"
#include "essh-string.c"
#include "list.c"
#include "dynamic_buffer.c"
#include "arena.c"
#include "hashtable.c"

#include "lexer.h"
#include "syntax_tree.c"
#include "globals.c"
#include "syntax_tree_allocator.c"


void addFile(char* filename, char* extension) {
    File file;
    u32 name_size = strlen(filename) + 1;
    file.filename = malloc(name_size);
    strcpy_s(file.filename, name_size, filename);

    list_add(parser.src_files, file);

    printf("addFile: \"%s\"\n", file.filename);
}

#include "error_messages.c"
#include "lexer.c"
#include "parser.c"
#include "validator.c"
#include "transpiler.c"
#include "interpreter.c"
#include "glsl_transpiler.c"


