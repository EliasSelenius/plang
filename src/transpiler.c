

static void transpileScope(Scope* scope);
static void transpileExpression(Expression* expr);
static void transpileStatement(Statement* statement);

static StringBuilder* sb;
static u32 tabing = 0;
static inline void newline() {
    sbAppend(sb, "\n");
    u32 t = tabing;
    while (t--) sbAppend(sb, "    ");
}

static char getCharFromU32(u32 num) {
    // TODO: do better than this...
    return '0' + num;
}

static char number_string[20]; // 20 is max char size for 64 bit integer
static StrSpan numberToString(u64 num) {
    if (num == 0) return (StrSpan) { "0", 1 };
    u32 strIndex = 20;

    while (num != 0) {
        u64 r = num % 10; num /= 10;
        number_string[--strIndex] = r + '0';
    }

    return (StrSpan) { &number_string[strIndex], 20 - strIndex };
}

static char* getTypeCname(Datatype type) {
    switch (type.kind) {

        // These should not exist at this point
        case Typekind_Invalid:
            printf("Attempted to transpile an invalid type. This is a bug!\n");
            return "$error_invalid";
        case Typekind_MustBeInfered:
            printf("Attempted to transpile a type that has not been infered. This is a bug!\n");
            return "$error_notinfered";
        case Typekind_AmbiguousInteger:
        case Typekind_AmbiguousDecimal:
            printf("Attempted to transpile an ambiguous type. This is a bug!\n");
            return "$error_ambiguous";
        case Typekind_Unresolved:
            printf("Attempted to transpile a type that should have been resolved. This is a bug!\n");
            return "$error_unresolved";

        case Typekind_uint8: return "uint8";
        case Typekind_uint16: return "uint16";
        case Typekind_uint32: return "uint32";
        case Typekind_uint64: return "uint64";
        case Typekind_int8: return "int8";
        case Typekind_int16: return "int16";
        case Typekind_int32: return "int32";
        case Typekind_int64: return "int64";
        case Typekind_float32: return "float32";
        case Typekind_float64: return "float64";
        case Typekind_void: return "void";
        case Typekind_char: return "char";

        case Typekind_Struct: return get_string(type.stru->name);
        case Typekind_Enum: return null;
        case Typekind_Alias: return get_string(type.alias->name);
        case Typekind_Opaque: return get_string(type.opaque_name);
        case Typekind_ProcPtr: return "SomeFuncPtr";
        case Typekind_Procedure: return "/*proc*/void";
    }
    return null;
}



static void transpileType(Datatype type) {

    if (type.kind == Typekind_ProcPtr) {
        sbAppend(sb, "proc_");
        sbAppendSpan(sb, numberToString(type.ref));
    } else {
        sbAppend(sb, getTypeCname(type));
    }

    u32 np = type.numPointers;
    while (np-- > 0) {
        sbAppend(sb, "*");
    }
}

static void transpileFuncCall(ProcCall* call) {

    // TODO: ad-hoc solution
    Datatype dealiased = dealiasType(call->proc_expr->datatype);
    if (dealiased.kind == Typekind_Procedure) {
        ProcSignature* sig = dealiased.procedure;
        sbAppend(sb, "((");
        transpileType(sig->return_type);
        sbAppend(sb, " (*)(");
        if (sig->arguments) {
            Argument* arg = sig->arguments;
            transpileType(arg->type);
            arg = arg->next;
            while (arg) {
                sbAppend(sb, ", ");
                transpileType(arg->type);
                arg = arg->next;
            }
        }
        sbAppend(sb, "))");
        transpileExpression(call->proc_expr);
        sbAppend(sb, ")");
    } else {
        transpileExpression(call->proc_expr);
        if (call->overload) sbAppendChar(sb, getCharFromU32(call->overload));
    }

    sbAppend(sb, "(");
    if (call->args) {
        transpileExpression(call->args[0]);

        u32 len = list_length(call->args);
        for (u32 i = 1; i < len; i++) {
            sbAppend(sb, ", ");
            transpileExpression(call->args[i]);
        }
    }
    sbAppend(sb, ")");
}

static char* getFormatTypeSpecifier(Datatype type) {
    Datatype datatype = dealiasType(type);

    if (datatype.numPointers == 1) {
        if (datatype.kind == Typekind_char) return "%s";

        return "%p";
    }

    if (datatype.numPointers == 0) {
        switch (datatype.kind) {
            case Typekind_AmbiguousDecimal: return "%f";
            case Typekind_AmbiguousInteger: return "%d";

            case Typekind_uint16: return "%hu";
            case Typekind_uint32: return "%u";
            case Typekind_uint64: return "%llu";
            case Typekind_int16: return "%hd";
            case Typekind_int32: return "%d";
            case Typekind_int64: return "%lld";

            case Typekind_float32: return "%f";
            case Typekind_float64: return "%Lf";

            case Typekind_char: return "%c";

            default: break;
        }
    }

    return "<print_error>";
}

static void transpilePrint(Expression** args) {
    sbAppend(sb, "printf(\"");

    foreach (item, args) {
        Expression* arg = *item;
        sbAppend(sb, getFormatTypeSpecifier(arg->datatype));
    }
    sbAppend(sb, "\"");

    foreach (arg, args) {
        sbAppend(sb, ", ");
        transpileExpression(*arg);
    }

    sbAppend(sb, ")");
}

static void transpileExpression(Expression* expr) {

    switch (expr->expressionType) {

        { // Binary & Unary Expressions

            case ExprType_Unary_PostIncrement: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                transpileExpression(unary->expr);
                sbAppend(sb, "++");
            } break;
            case ExprType_Unary_PostDecrement: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                transpileExpression(unary->expr);
                sbAppend(sb, "--");
            } break;


            char* operator = null;

            case ExprType_Unary_PreIncrement: operator = "++"; goto unary;
            case ExprType_Unary_PreDecrement: operator = "--"; goto unary;
            case ExprType_Unary_Not:          operator = "!"; goto unary;
            case ExprType_Unary_AddressOf:    operator = "&"; goto unary;
            case ExprType_Unary_ValueOf:      operator = "*"; goto unary;
            case ExprType_Unary_Negate:       operator = "-"; goto unary;
            case ExprType_Unary_BitwiseNot:   operator = "~"; goto unary;

            case ExprType_Bitwise_And: operator = " & "; goto binary;
            case ExprType_Bitwise_Or: operator = " | "; goto binary;
            case ExprType_Bitwise_Xor: operator = " ^ "; goto binary;
            case ExprType_Bitwise_Lshift: operator = " << "; goto binary;
            case ExprType_Bitwise_Rshift: operator = " >> "; goto binary;

            case ExprType_Plus:  operator = " + "; goto binary;
            case ExprType_Minus: operator = " - "; goto binary;
            case ExprType_Mul:   operator = " * "; goto binary;
            case ExprType_Div:   operator = " / "; goto binary;
            case ExprType_Mod:   operator = " % "; goto binary;

            case ExprType_Less:          operator = " < ";  goto binary;
            case ExprType_Greater:       operator = " > ";  goto binary;
            case ExprType_LessEquals:    operator = " <= "; goto binary;
            case ExprType_GreaterEquals: operator = " >= "; goto binary;
            case ExprType_Equals:        operator = " == "; goto binary;
            case ExprType_NotEquals:     operator = " != "; goto binary;
            case ExprType_BooleanAnd:    operator = " && "; goto binary;
            case ExprType_BooleanOr:     operator = " || "; goto binary;

            unary: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                sbAppend(sb, operator);
                transpileExpression(unary->expr);
            } break;

            binary: {
                BinaryExpression* bop = (BinaryExpression*)expr;
                sbAppend(sb, "(");
                transpileExpression(bop->left);
                sbAppend(sb, operator);
                transpileExpression(bop->right);
                sbAppend(sb, ")");
            } break;
        }

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            transpileExpression(deref->expr);
            if (deref->expr->datatype.numPointers) sbAppend(sb, "->");
            else sbAppend(sb, ".");
            sbAppend(sb, get_string(deref->name));
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;
            transpileExpression(ind->indexed);
            sbAppend(sb, "[");
            transpileExpression(ind->index);
            sbAppend(sb, "]");
        } break;

        case ExprType_Literal_Null: {
            sbAppend(sb, "0");
        } break;

        case ExprType_Literal_String: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppend(sb, "\"");
            sbAppend(sb, get_string(lit->string));
            sbAppend(sb, "\"");
        } break;

        case ExprType_Literal_Char: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendChar(sb, '\'');
            sbAppendChar(sb, lit->character);
            sbAppendChar(sb, '\'');
        } break;

        case ExprType_Literal_True: sbAppend(sb, "1"); break;
        case ExprType_Literal_False: sbAppend(sb, "0"); break;

        case ExprType_Literal_Integer: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendSpan(sb, numberToString(lit->integer));
        } break;

        case ExprType_Literal_Decimal: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            char temp_buffer[24];
            u32 i = snprintf(temp_buffer, 23, "%f", lit->decimal);
            temp_buffer[i] = '\0';
            sbAppend(sb, temp_buffer);
        } break;

        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;

            Identifier name = 0;

            switch (var->ref.reftype) {
                case RefType_Invalid: name = var->ref.name; break; // TODO: temporary
                case RefType_Procedure: name = var->ref.procedure->name; break;
                case RefType_Global: name = var->ref.global->name; break;
                case RefType_Constant: name = var->ref.constant->name; break;
            }

            sbAppend(sb, get_string(name));
        } break;

        case ExprType_Alloc: {
            AllocExpression* allocExpr = (AllocExpression*)expr;
            sbAppend(sb, "malloc(sizeof(");
            transpileType(allocExpr->type);
            sbAppend(sb, ")");
            if (allocExpr->sizeExpr) {
                sbAppend(sb, " * ");
                transpileExpression(allocExpr->sizeExpr);
            }
            sbAppend(sb, ")");
        } break;

        case ExprType_Sizeof: {
            SizeofExpression* sof = (SizeofExpression*)expr;
            sbAppend(sb, "sizeof(");
            transpileType(sof->type);
            sbAppend(sb, ")");
        } break;

        case ExprType_Cast: {
            CastExpression* cast = (CastExpression*)expr;
            sbAppend(sb, "(");
            transpileType(cast->castToType);
            sbAppend(sb, ")");
            transpileExpression(cast->expr);
        } break;

        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            transpileExpression(ter->condition);
            sbAppend(sb, " ? ");
            transpileExpression(ter->thenExpr);
            sbAppend(sb, " : ");
            transpileExpression(ter->elseExpr);
        } break;

        case ExprType_ProcCall: {
            ProcCall* fc = (ProcCall*)expr;

            if (fc->proc_expr->expressionType == ExprType_Variable) {
                VariableExpression* var = (VariableExpression*)fc->proc_expr;
                if (var->ref.reftype == RefType_Invalid) {
                    Identifier name = var->ref.name;
                    if (name == builtin_print_name) {
                        transpilePrint(fc->args);
                        break;
                    }
                }
            }

            transpileFuncCall(fc);
        } break;

        case ExprType_Parenthesized: {
            ParenthesizedExpression* p = (ParenthesizedExpression*)expr;
            if (isBinaryExpression(p->innerExpr)) {
                transpileExpression(p->innerExpr);
            } else {
                sbAppend(sb, "(");
                transpileExpression(p->innerExpr);
                sbAppend(sb, ")");
            }
        } break;
    }

}

static void transpileCondition(Expression* cond) {
    if (cond->expressionType == ExprType_Parenthesized || isBinaryExpression(cond)) {
        transpileExpression(cond);
    } else {
        sbAppend(sb, "(");
        transpileExpression(cond);
        sbAppend(sb, ")");
    }
}

static void transpileIfStatement(IfStatement* ifst) {
    if (ifst->condition) {
        sbAppend(sb, "if ");
        transpileCondition(ifst->condition);
        sbAppend(sb, " ");
    }

    transpileStatement(ifst->statement);

    if (ifst->next) {
        sbAppend(sb, " else ");
        transpileIfStatement(ifst->next);
    }
}

static void transpileVarDecl(VarDecl* decl) {
    if (decl->base.statementType == Statement_FixedArray_Declaration) {
        decl->type.numPointers--;
        transpileType(decl->type);
        decl->type.numPointers++;
        sbAppend(sb, " ");
        sbAppend(sb, get_string(decl->name));

        sbAppend(sb, "[");
        transpileExpression(decl->assignmentOrNull);
        sbAppend(sb, "]");
    } else {
        transpileType(decl->type);
        sbAppend(sb, " ");
        sbAppend(sb, get_string(decl->name));

        if (decl->assignmentOrNull) {
            sbAppend(sb, " = ");
            transpileExpression(decl->assignmentOrNull);
        }
    }

    sbAppend(sb, ";");
}

static void transpileStatement(Statement* statement) {
    switch (statement->statementType) {
        case Statement_FixedArray_Declaration:
        case Statement_Declaration: {
            VarDecl* decl = (VarDecl*)statement;
            transpileVarDecl(decl);
        } break;
        case Statement_Assignment: {
            Assignement* ass = (Assignement*)statement;
            transpileExpression(ass->assigneeExpr);

            switch (ass->assignmentOper) {
                case Tok_Assign: sbAppend(sb, " = "); break;
                case Tok_PlusAssign: sbAppend(sb, " += "); break;
                case Tok_MinusAssign: sbAppend(sb, " -= "); break;
                case Tok_MulAssign: sbAppend(sb, " *= "); break;
                case Tok_DivAssign: sbAppend(sb, " /= "); break;
                case Tok_BitAndAssign: sbAppend(sb, " &= "); break;
                case Tok_BitOrAssign: sbAppend(sb, " |= "); break;
                case Tok_BitXorAssign: sbAppend(sb, " ^= "); break;
                default: break;
            }

            transpileExpression(ass->expr);
            sbAppend(sb, ";");
        } break;

        case Statement_Scope: {
            transpileScope((Scope*)statement);
        } break;
        case Statement_If: {
            transpileIfStatement((IfStatement*)statement);
        } break;
        case Statement_While: {
            WhileStatement* sta = (WhileStatement*)statement;
            sbAppend(sb, "while ");
            transpileCondition(sta->condition);
            sbAppend(sb, " ");
            transpileStatement(sta->statement);
        } break;
        case Statement_ForIn: {
            ForInStatement* forin = (ForInStatement*)statement;
            char* name = get_string(forin->index_name);

            sbAppend(sb, "for (");
            transpileType(forin->index_type);
            sbAppend(sb, " ");
            sbAppend(sb, name);
            sbAppend(sb, " = ");
            transpileExpression(forin->min_expr);
            sbAppend(sb, "; ");

            sbAppend(sb, name);
            sbAppend(sb, " < ");
            transpileExpression(forin->max_expr);
            sbAppend(sb, "; ");

            sbAppend(sb, name);
            sbAppend(sb, "++) ");

            transpileStatement(forin->statement);
        } break;

        case Statement_Switch: {
            SwitchStatement* switchSta = (SwitchStatement*)statement;
            sbAppend(sb, "switch ");
            transpileCondition(switchSta->expr);
            sbAppend(sb, " ");
            transpileScope(switchSta->scope);
        } break;

        case Statement_LocalProc: {
            sbAppend(sb, "// local procedure");
        } break;

        case Statement_Break: sbAppend(sb, "break;"); break;
        case Statement_Continue: sbAppend(sb, "continue;"); break;

        case Statement_Return: {
            ReturnStatement* retSta = (ReturnStatement*)statement;
            if (retSta->returnExpr) {
                sbAppend(sb, "return ");
                transpileExpression(retSta->returnExpr);
                sbAppend(sb, ";");
            } else {
                sbAppend(sb, "return;");
            }

        } break;

        case Statement_Goto: {
            GotoStatement* go = (GotoStatement*)statement;
            sbAppend(sb, "goto ");
            sbAppend(sb, get_string(go->label));
            sbAppend(sb, ";");
        } break;
        case Statement_Label: {
            LabelStatement* l = (LabelStatement*)statement;
            sbAppend(sb, get_string(l->label));
            sbAppend(sb, ":");
        } break;
        case Statement_CaseLabel: {
            CaseLabelStatement* caseLabel = (CaseLabelStatement*)statement;
            sbAppend(sb, "case ");
            transpileExpression(caseLabel->expr);
            sbAppend(sb, ":");
        } break;
        case Statement_DefaultLabel: {
            sbAppend(sb, "default:");
        } break;

        case Statement_Expression: {
            StatementExpression* staExpr = (StatementExpression*)statement;
            transpileExpression(staExpr->expr);
            sbAppend(sb, ";");
        } break;
    }
}

static void transpileScope(Scope* scope) {
    sbAppend(sb, "{");
    tabing++;

    u32 len = list_length(scope->statements);
    for (u32 i = 0; i < len; i++) {
        newline();
        transpileStatement(scope->statements[i]);
    }

    tabing--;
    newline();

    sbAppend(sb, "}");
}

static void transpileLocalProc(LocalProc* localproc) {
    transpileType(localproc->proc.returnType);
    sbAppend(sb, " ");
    sbAppend(sb, get_string(localproc->proc.name));
    sbAppend(sb, "(");
    foreach (captured, localproc->captures) {
        Datatype type = captured->type;
        type.numPointers++;
        transpileType(type);
        sbAppend(sb, " ");
        sbAppend(sb, get_string(captured->name));
        sbAppend(sb, ", ");
    }
    if (localproc->proc.arguments) {
        foreach (arg, localproc->proc.arguments) {
            transpileType(arg->type);
            sbAppend(sb, " ");
            sbAppend(sb, get_string(arg->name));
            sbAppend(sb, ", ");
        }
    }

    sb->length -= 2;

    sbAppend(sb, ") ");
    transpileScope(localproc->proc.scope);
    newline();
}

static void transpileFunctionSignature(Procedure* proc) {
    transpileType(proc->returnType);
    sbAppend(sb, " ");
    sbAppend(sb, get_string(proc->name));
    if (proc->overload) sbAppendChar(sb, getCharFromU32(proc->overload));
    sbAppend(sb, "(");
    if (proc->arguments) {

        ProcArg arg = proc->arguments[0];
        transpileType(arg.type);
        sbAppend(sb, " ");
        sbAppend(sb, get_string(arg.name));

        u32 len = list_length(proc->arguments);
        for (u32 i = 1; i < len; i++) {
            sbAppend(sb, ", ");

            arg = proc->arguments[i];
            transpileType(arg.type);
            sbAppend(sb, " ");
            sbAppend(sb, get_string(arg.name));
        }
    }
    sbAppend(sb, ")");
}

static void transpileProcedure(Procedure* proc) {

    { // transpile local procs first
        foreach (sta, proc->scope->statements) {
            if ((*sta)->statementType == Statement_LocalProc) {
                LocalProc* localproc = (LocalProc*)(*sta);
                transpileLocalProc(localproc);
            }
        }
    }

    transpileFunctionSignature(proc);
    sbAppend(sb, " ");
    transpileScope(proc->scope);
    newline();
}

static void transpileStruct(PlangStruct* stru) {
    sbAppend(sb, "typedef struct ");
    sbAppend(sb, get_string(stru->name));
    sbAppend(sb, " {");

    tabing++;

    u32 len = list_length(stru->fields);
    for (u32 i = 0; i < len; i++) {
        newline();
        transpileType(stru->fields[i].type);
        sbAppend(sb, " ");
        sbAppend(sb, get_string(stru->fields[i].name));
        sbAppend(sb, ";");
    }

    tabing--;
    newline();

    sbAppend(sb, "} ");
    sbAppend(sb, get_string(stru->name));
    sbAppend(sb, ";\n");
}

static u32 countStructDependencies(PlangStruct* stru) {
    u32 deps = 0;
    u32 fieldsLen = list_length(stru->fields);
    for (u32 f = 0; f < fieldsLen; f++) {
        Datatype datatype = stru->fields[f].type;
        if (datatype.numPointers) continue;
        if (datatype.kind != Typekind_Struct) continue;

        deps++;
        deps += countStructDependencies(datatype.stru);
    }

    return deps;
}

static void transpileFuncptrType(Datatype type) {
    if (type.kind == Typekind_Alias) {
        Datatype dealiased = dealiasType(type);
        if (dealiased.kind == Typekind_ProcPtr) {
            transpileType(dealiased);
            return;
        }
    }

    transpileType(type);
}


static void transpile() {
    // TODO: use a higer initial capacity for the string builder
    StringBuilder builder = sbCreate();
    sb = &builder;

    // sbAppend(sb, "#include <stdlib.h>\n"); // malloc
    // sbAppend(sb, "#include <stdio.h>\n"); // printf
    // sbAppend(sb, "#define true 1\n#define false 0\n");

    current_namespace = g_Codebase.namespaces[0];

    { // namespaces
        sbAppend(sb, "/* Namespaces\n");
        foreach(item, g_Codebase.namespaces) {
            Namespace* ns = *item;
            sbAppend(sb, "item: ");
            sbAppend(sb, get_string(ns->name));
            sbAppend(sb, "\n");
        }

        sbAppend(sb, "*/\n");
    }

    { // syntax tree
        u32 length = list_length(g_Codebase.string_table.byteoffsets);
        sbAppend(sb, "static char string_table[] = \"");
        for (u32 i = 0; i < length; i++) {
            char* str = get_string_byindex(i);
            sbAppend(sb, str);
            sbAppend(sb, "\\0");
        }
        sbAppend(sb, "\";\n");
    }


    sbAppend(sb, "\n// types\n");
    sbAppend(sb, "typedef signed char int8;\n");
    sbAppend(sb, "typedef signed short int16;\n");
    sbAppend(sb, "typedef signed int int32;\n");
    sbAppend(sb, "typedef signed long long int64;\n");
    sbAppend(sb, "typedef unsigned char uint8;\n");
    sbAppend(sb, "typedef unsigned short uint16;\n");
    sbAppend(sb, "typedef unsigned int uint32;\n");
    sbAppend(sb, "typedef unsigned long long uint64;\n");
    sbAppend(sb, "typedef float float32;\n");
    sbAppend(sb, "typedef double float64;\n");

    sbAppend(sb, "\n// Opaque types\n");
    iterate(opaque_types, {
        char* typename = get_string(*item);
        sbAppend(sb, "typedef struct ");
        sbAppend(sb, typename);
        sbAppend(sb, " ");
        sbAppend(sb, typename);
        sbAppend(sb, ";\n");
    });

    sbAppend(sb, "\n// Structs forward declarations\n");
    iterate(structs, {
        char* name = get_string(item->name);
        sbAppend(sb, "typedef struct ");
        sbAppend(sb, name);
        sbAppend(sb, " ");
        sbAppend(sb, name);
        sbAppend(sb, ";\n");
    });

    sbAppend(sb, "\n// Type aliases\n");
    iterate(aliases, {
        char* typename = get_string(item->name);
        sbAppend(sb, "typedef ");
        transpileType(item->aliasedType);
        sbAppend(sb, " ");
        sbAppend(sb, typename);
        sbAppend(sb, ";\n");
    });


    {
        sbAppend(sb, "\n// Structs\n");
        u32 structs_count = 0;
        iterate(structs, {
            item->deps = countStructDependencies(item);
            structs_count++;
        });

        u32 dep = 0;
        u32 transpiled = 0;
        while (transpiled < structs_count) {
            iterate(structs, {
                if (item->deps == dep) {
                    transpileStruct(item);
                    transpiled++;
                }
            });
            dep++;
        }
    }

    sbAppend(sb, "\n// Forward declarations\n");
    iterate(procedures, {
        transpileFunctionSignature(item);
        sbAppend(sb, ";\n");
    });

    sbAppend(sb, "\n// Constants\n");
    iterate(constants, {
        sbAppend(sb, "#define ");
        sbAppend(sb, get_string(item->name));
        sbAppend(sb, " ");
        transpileExpression(item->expr);
        sbAppend(sb, "\n");
    });

    sbAppend(sb, "\n// Globals\n");
    iterate(global_variables, {
        transpileVarDecl(item);
        sbAppend(sb, "\n");
    });

    sbAppend(sb, "\n// Implementations\n");
    iterate(procedures, {
        if (!item->scope) continue;
        transpileProcedure(item);
    });


/*
    { // opaque types
        sbAppend(sb, "\n// Opaque types\n");
        foreach (optype, current_namespace->opaque_types) {
            char* typename = get_string(*optype);
            sbAppend(sb, "typedef struct ");
            sbAppend(sb, typename);
            sbAppend(sb, " ");
            sbAppend(sb, typename);
            sbAppend(sb, ";\n");
        }
    }

    { // struct typedefs
        sbAppend(sb, "\n// Structs forward declarations\n");
        foreach (stru, current_namespace->structs) {
            char* name = get_string(stru->name);
            sbAppend(sb, "typedef struct ");
            sbAppend(sb, name);
            sbAppend(sb, " ");
            sbAppend(sb, name);
            sbAppend(sb, ";\n");
        }
    }

    { // type aliases (except funcptrs)
        sbAppend(sb, "\n// Type aliases\n");
        foreach (alias, current_namespace->aliases) {
            if (alias->aliasedType.kind == Typekind_ProcPtr) continue;
            char* typename = get_string(alias->name);
            sbAppend(sb, "typedef ");
            transpileType(alias->aliasedType);
            sbAppend(sb, " ");
            sbAppend(sb, typename);
            sbAppend(sb, ";\n");
        }
    }


    // { // Func ptrs
    //     sbAppend(sb, "\n// Function pointers\n");
    //     u32 i = 0;
    //     while (i < g_Codebase.arena_procedure_types.allocated) {
    //         ProcPtr* f = getProcPtr(i);
    //         sbAppend(sb, "typedef ");
    //         transpileFuncptrType(f->return_type);
    //         sbAppend(sb, " ");
    //         sbAppend(sb, "proc_");
    //         sbAppendSpan(sb, numberToString(i));
    //         sbAppend(sb, "(");
    //         if (f->arg_count) {
    //             transpileFuncptrType(f->arg_types[0]);
    //             for (u32 i = 1; i < f->arg_count; i++) {
    //                 sbAppend(sb, ", ");
    //                 transpileFuncptrType(f->arg_types[i]);
    //             }
    //         }
    //         sbAppend(sb, ");\n");
    //         i += sizeof(ProcPtr) + sizeof(Datatype) * f->arg_count;
    //     }
    // }

    // { // type aliases (funcptrs only)
    //     sbAppend(sb, "\n// Function pointer aliases\n");
    //     foreach (alias, current_namespace->aliases) {
    //         if (alias->aliasedType.kind != Typekind_ProcPtr) continue;
    //         char* typename = get_string(alias->name);
    //         sbAppend(sb, "typedef ");
    //         transpileType(alias->aliasedType);
    //         sbAppend(sb, " ");
    //         sbAppend(sb, typename);
    //         sbAppend(sb, ";\n");
    //     }
    // }


    { // structs
        sbAppend(sb, "\n// Structs\n");
        u32 structsLen = list_length(current_namespace->structs);
        u32* deps = malloc(sizeof(u32) * structsLen);

        for (u32 i = 0; i < structsLen; i++) {
            PlangStruct* stru = &current_namespace->structs[i];

            deps[i] = countStructDependencies(stru);
        }

        u32 dep = 0;
        u32 transpiled = 0;
        while (transpiled < structsLen) {
            for (u32 i = 0; i < structsLen; i++) {
                if (deps[i] == dep) {
                    PlangStruct* stru = &current_namespace->structs[i];
                    char buffer[16];
                    sprintf_s(buffer, 16, "// deps: %d\n", deps[i]);
                    sbAppend(sb, buffer);
                    transpileStruct(stru);
                    transpiled++;
                }
            }
            dep++;
        }

        free(deps);
    }

    sbAppend(sb, "\n// Forward declarations\n");
    u32 functionsLen = list_length(current_namespace->procedures);
    for (u32 i = 0; i < functionsLen; i++) {
        Procedure* proc = &current_namespace->procedures[i];

        transpileFunctionSignature(proc);
        sbAppend(sb, ";\n");
    }

    sbAppend(sb, "\n// Constants\n");
    foreach (cont, current_namespace->constants) {
        sbAppend(sb, "#define ");
        sbAppend(sb, get_string(cont->name));
        sbAppend(sb, " ");
        transpileExpression(cont->expr);
        sbAppend(sb, "\n");
    }

    sbAppend(sb, "\n// Globals\n");
    u32 globLen = list_length(current_namespace->global_variables);
    for (u32 i = 0; i < globLen; i++) {
        transpileVarDecl(&current_namespace->global_variables[i]);
        sbAppend(sb, "\n");
    }

    sbAppend(sb, "\n// Implementations\n");

    for (u32 i = 0; i < functionsLen; i++) {
        Procedure* proc = &current_namespace->procedures[i];
        if (!proc->scope) continue;
        transpileProcedure(proc);
    }
*/

    FILE* file;
    if ( !fopen_s(&file, "output.g.c", "w") ) {
        fprintf(file, "%s", sb->content);
        fclose(file);
    } else {
        printf("Could not write to output.g.c\n");
    }


    sbDestroy(sb);
}