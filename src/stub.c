
#include <stdlib.h> // included for malloc
#include <string.h> // included for strlen()
#include <stdio.h>  // printf
#include <stdarg.h>
#include <math.h>
#include <assert.h>


#include "prelude.h"

#include "essh-string.h"
#include "essh-string.c"
#include "list.c"
#include "dynamic_buffer.c"

#include "lexer.h"
#include "syntax_tree.c"
#include "globals.c"
#include "syntax_tree_allocator.c"

#include "error_messages.c"
#include "lexer.c"
#include "parser.c"
#include "validator.c"
#include "transpiler.c"

#include "platform.h"

#include "io.c"
#include "main.c"

