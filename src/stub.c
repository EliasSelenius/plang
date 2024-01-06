
#include <stdlib.h> // included for malloc
#include <string.h> // included for strlen()
#include <stdio.h>  // printf
#include <stdarg.h>
#include <math.h>
#include <assert.h>


#include "prelude.h"
#include "io.c"
#include "../platform/platform.h"

#include "essh-string.h"
#include "essh-string.c"
#include "list.c"
#include "dynamic_buffer.c"
#include "arena.c"
#include "hashtable.c"

#include "public.h"
#include "lexer.h"
#include "syntax_tree.c"
#include "globals.c"
#include "syntax_tree_allocator.c"


#include "error_messages.c"
#include "lexer.c"
#include "parser.c"
#include "parser_expressions.c"
#include "parser_statements.c"
#include "parse_c_file.c"
#include "validator.c"
#include "transpiler.c"
#include "interpreter.c"
#include "glsl_transpiler.c"


