

typedef struct C_Transpiler {
    Declaration** static_decls; // list
    StringBuilder sb;
    u32 tabing;
} C_Transpiler;

#define tr_write(c_str) sbAppend(&tr->sb, c_str)
#define tr_writef(format, ...) sb_append_format(&tr->sb, format, __VA_ARGS__)


static inline void newline(C_Transpiler* tr) {
    tr_write("\n");
    u32 t = tr->tabing;
    while (t--) tr_write("    ");
}

static void transpile_procedure(C_Transpiler* tr, Procedure* proc);
static void transpile_node(C_Transpiler* tr, NodeRef p);


static char* getTypeCname(Datatype type) {
    switch (type.kind) {

        // These should not exist at this point
        case Typekind_Invalid:
            printf("Attempted to transpile an invalid type. This is a bug!\n");
            return "$error_invalid";
        case Typekind_AmbiguousInteger:
        case Typekind_AmbiguousDecimal:
            printf("Attempted to transpile an ambiguous type. This is a bug!\n");
            return "$error_ambiguous";

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
        case Typekind_Enum: return get_string(type._enum->name);
        case Typekind_Typedef: return get_string(type.type_def->name);
        case Typekind_Procedure: return null;

        case Typekind_Array: return null;
        case Typekind_Fixed_Array: return null;
        case Typekind_Dynamic_Array: return null;
    }
    return null;
}

static void _transpileDatatype(C_Transpiler* tr, Datatype type, Identifier name);
static void transpileDatatype(C_Transpiler* tr, Datatype type);
static void transpileType(C_Transpiler* tr, Type* type) { transpileDatatype(tr, type->solvedstate); }

static void transpileProcSigStart(C_Transpiler* tr, Type* proc_type) {
    if (proc_type->procedure.return_type->solvedstate.kind == Typekind_Procedure) {
        transpileProcSigStart(tr, proc_type->procedure.return_type);
    } else {
        transpileDatatype(tr, proc_type->procedure.return_type->solvedstate);
        tr_write(" ");
    }

    tr_write("(");
    u32 np = proc_type->solvedstate.numPointers;
    while (np-- > 0) tr_write("*");
}

static void transpileProcArguments(C_Transpiler* tr, Type* proc_type) {
    tr_write("(");
    Type* arg = proc_type->procedure.first_argument;
    while (arg) {
        transpileDatatype(tr, arg->solvedstate);
        if (arg->next) tr_write(", ");
        arg = arg->next;
    }
    tr_write(")");
}

static void transpileProcSigEnd(C_Transpiler* tr, Type* proc_type) {
    tr_write(")");
    transpileProcArguments(tr, proc_type);

    while (proc_type->procedure.return_type->solvedstate.kind == Typekind_Procedure) {
        tr_write(")");
        proc_type = proc_type->procedure.return_type;
        transpileProcArguments(tr, proc_type);
    }
}

static void transpileTypeStart(C_Transpiler* tr, Datatype type) {

    if (type.kind == Typekind_Array) {
        tr_write("Array");

        u32 np = type.numPointers;
        while (np-- > 0) tr_write("*");
        return;
    }

    if ((type.kind == Typekind_Fixed_Array) ||
        (type.kind == Typekind_Dynamic_Array)) {

        transpileTypeStart(tr, type.array_typenode->array.element_type->solvedstate);
        tr_write("*");
        return;
    }


    if (type.kind == Typekind_Procedure) {
        transpileProcSigStart(tr, type.proc_ptr_typenode);
    } else {
        tr_write(getTypeCname(type));
        u32 np = type.numPointers;
        while (np-- > 0) tr_write("*");
        //tr_write(" ");
    }
}

static void transpileTypeEnd(C_Transpiler* tr, Datatype type) {
    if (type.kind == Typekind_Procedure) transpileProcSigEnd(tr, type.proc_ptr_typenode);
}


/*
    void *name
        name is pointer
    void (*name)()
        name is pointer to function() returning void
    void (*name)(int)
        name is pointer to function(int) returning void
    void (*(*name)(int))(int)
        name is pointer to function(int) returning pointer to function(int) returning void
*/

static void transpileDatatype(C_Transpiler* tr, Datatype type) {
    transpileTypeStart(tr, type);
    transpileTypeEnd(tr, type);
}

static void _transpileDatatype(C_Transpiler* tr, Datatype type, Identifier name) {
    transpileTypeStart(tr, type);
    if (name && type.kind != Typekind_Procedure) tr_write(" ");
    tr_write(get_string(name));
    transpileTypeEnd(tr, type);
}



// static void transpile_declarator(Datatype datatype, Identifier name) {
//     if (datatype.kind == Typekind_Procedure) {

//         tr_write("(");
//         transpile_declarator(datatype.proc_ptr_typenode->procedure.return_type, name);
//         tr_write(")");

//         tr_write("(");
//         // args
//         tr_write(")");
//     } else {
//         u32 np = datatype.numPointers;
//         while (np-- > 0) tr_write("*");
//         tr_write(get_string(name));
//     }
// }

// static void transpile_procptr(Type* proc_type) {
//     if (proc_type->procedure.return_type->node_type == TypeNode_Procedure) {
//         transpile_procptr(proc_type->procedure.return_type);
//     }

//     tr_write("(*");
// }

// static void transpile_cdecl(Datatype datatype, Identifier name_or_null) {

//     Datatype type_specifier = datatype;
//     while (true) {
//         if (type_specifier.kind == Typekind_Procedure) {
//             type_specifier = type_specifier.proc_ptr_typenode->procedure.return_type->solvedstate;
//         } else {
//             tr_write(getTypeCname(type_specifier));
//             u32 np = type_specifier.numPointers;
//             while (np-- > 0) tr_write("*");
//             break;
//         }
//     }
// }


static void transpilePrintFormat(C_Transpiler* tr, Datatype type) {
    type = dealiasType(type);

    if (type.numPointers == 1) {
        if (type.kind == Typekind_char) { tr_write("%s"); return; }
    }

    if (type.numPointers) {
        tr_write("%p");
        return;
    }


    switch (type.kind) {
        case Typekind_AmbiguousDecimal: tr_write("%f"); return;
        case Typekind_AmbiguousInteger: tr_write("%d"); return;

        case Typekind_uint16: tr_write("%hu"); return;
        case Typekind_uint32: tr_write("%u"); return;
        case Typekind_uint64: tr_write("%llu"); return;
        case Typekind_int16: tr_write("%hd"); return;
        case Typekind_int32: tr_write("%d"); return;
        case Typekind_int64: tr_write("%lld"); return;

        case Typekind_float32: tr_write("%f"); return;
        case Typekind_float64: tr_write("%Lf"); return;
        case Typekind_char: tr_write("%c"); return;

        default: break;
    }

    if (type.kind == Typekind_Struct) {
        Struct* stru = type.stru;
        tr_write("{");
        foreach (field, stru->fields) {
            tr_write(get_string(field->name));
            tr_write(" = ");
            transpilePrintFormat(tr, field->type->solvedstate);
            tr_write(", ");
        }
        tr->sb.length -= 2;
        tr_write("}");
        return;
    }

    tr_write("<print_error>");
}

static void transpile_print_call(C_Transpiler* tr, ProcCallExpression* call) {
    tr_write("printf(\"");
    {
        foreach (item, call->args) transpilePrintFormat(tr, item->expr->datatype);
    }
    tr_write("\"");


    foreach (item, call->args) {
        Expression* arg = item->expr;
        tr_write(", ");
        if (arg->datatype.kind == Typekind_Struct && arg->datatype.numPointers == 0) {
            foreach (field, arg->datatype.stru->fields) {

                if (field->type->solvedstate.kind == Typekind_Struct && field->type->solvedstate.numPointers == 0) {
                    // TODO: print struct inside struct
                }

                transpile_node(tr, *item);
                tr_writef(".%s, ", get_string(field->name));
            }
            tr->sb.length -= 2;
        } else {
            if (arg->kind == Node_Sizeof) tr_write("(uint32)");
            transpile_node(tr, *item);
        }
    }

    tr_write(")");
}

static void transpile_proccall(C_Transpiler* tr, ProcCallExpression* call) {

    // special cases:
    if (call->proc_expr.node->kind == Node_Variable) {
        if (call->proc_expr.Variable->name == builtin_string_print) {
            transpile_print_call(tr, call);
            return;
        }
    }


    transpile_node(tr, call->proc_expr);
    if (call->proc && call->proc->overload) tr_writef("%d", call->proc->overload);

    tr_write("(");
    if (call->args) {
        transpile_node(tr, call->args[0]);

        u32 len = list_length(call->args);
        for (u32 i = 1; i < len; i++) {
            tr_write(", ");
            transpile_node(tr, call->args[i]);
        }
    }
    tr_write(")");
}

static void transpile_condition_expr(C_Transpiler* tr, NodeRef ref) {
    if (ref.node->kind == Node_Parenthesized || is_binary_expr(ref)) transpile_node(tr, ref);
    else { tr_write("("); transpile_node(tr, ref); tr_write(")"); }
}


static void transpile_struct(C_Transpiler* tr, Struct* stru) {
    tr_writef("struct %s {", get_string(stru->name));
    tr->tabing++;

    foreach (field, stru->fields) {
        newline(tr);
        _transpileDatatype(tr, field->type->solvedstate, field->name);
        tr_write(";");
    }

    tr->tabing--; newline(tr);
    tr_write("}");
}

static void transpile_typedef(C_Transpiler* tr, Typedef* def) {
    tr_write("typedef ");
    if (def->type) {
        _transpileDatatype(tr, def->type->solvedstate, def->name);
    } else {
        char* name = get_string(def->name);
        tr_writef("struct %s %s", name, name);
    }
    tr_write(";");
}

static void transpile_declaration(C_Transpiler* tr, Declaration* decl) {
    if (decl->is_static) {
        list_add(tr->static_decls, decl);
        tr_write("// static decl");
        return;
    }

    _transpileDatatype(tr, decl->type->solvedstate, decl->name);
    if (decl->expr.node) {
        tr_write(" = ");
        transpile_node(tr, decl->expr);
    }
}

static bool should_terminate_with_semicolon(NodeRef p) {
    switch (p.node->kind) {
        case Node_SwitchStmt:
        case Node_Scope: return false;

        case Node_IfStmt:
            if (p.IfStmt->else_statement.node) return should_terminate_with_semicolon(p.IfStmt->else_statement);
            if (p.IfStmt->then_statement.node) return should_terminate_with_semicolon(p.IfStmt->then_statement);
            return true;
        case Node_WhileStmt: if (p.WhileStmt->statement.node) return should_terminate_with_semicolon(p.WhileStmt->statement); else return true;
        case Node_ForStmt: if (p.ForStmt->statement.node) return should_terminate_with_semicolon(p.ForStmt->statement); else return true;

        default: return true;
    }
}

static void transpile_semicolon_after_node(C_Transpiler* tr, NodeRef p) {
    if (p.node != null && should_terminate_with_semicolon(p) == false) return;
    tr_write(";");
}

static void transpile_scope(C_Transpiler* tr, Scope* scope) {
    tr_write("{"); tr->tabing++;

    foreach (refp, scope->statements) {
        newline(tr);
        transpile_node(tr, *refp);
        transpile_semicolon_after_node(tr, *refp);
    }

    tr->tabing--; newline(tr);
    tr_write("}");
}

static void transpile_proc_signature(C_Transpiler* tr, Procedure* proc) {

    if (proc->name != builtin_string_main && proc->scope) tr_write("static ");
    transpileType(tr, proc->return_type);
    tr_write(" ");

    if (proc->name == builtin_string_main) tr_write("__main");
    else tr_write(get_string(proc->name));

    if (proc->overload) tr_writef("%u", proc->overload);

    tr_write("(");
    if (proc->arguments) {
        foreach (arg, proc->arguments) {
            _transpileDatatype(tr, arg->type->solvedstate, arg->name);
            tr_write(", ");
        }
        tr->sb.length -= 2;
    }
    tr_write(")");
}

static void transpile_local_procedures(C_Transpiler* tr, NodeRef ref) {
    if (ref.node == null) return;
    if (ref.node->kind != Node_Scope) return;

    foreach (s, ref.Scope->statements) {
        switch (s->node->kind) {
            case Node_Procedure: transpile_procedure(tr, s->Procedure); break;
            case Node_Scope:     transpile_local_procedures(tr, *s); break;
            case Node_IfStmt:    transpile_local_procedures(tr, s->IfStmt->then_statement);
                                 transpile_local_procedures(tr, s->IfStmt->else_statement); break;
            case Node_WhileStmt: transpile_local_procedures(tr, s->WhileStmt->statement); break;
            case Node_ForStmt:   transpile_local_procedures(tr, s->ForStmt->statement); break;
            default: break;
        }
    }
}

static void transpile_procedure(C_Transpiler* tr, Procedure* proc) {
    transpile_local_procedures(tr, (NodeRef)proc->scope);
    transpile_proc_signature(tr, proc);
    tr_write(" ");
    transpile_scope(tr, proc->scope);
    newline(tr);
}

static void transpile_for_loop(C_Transpiler* tr, ForStmt* forsta) {
    tr_write("for (");
    if (forsta->index_type) transpileType(tr, forsta->index_type);
    else transpileDatatype(tr, default_for_loop_numeric_type);

    char* name = get_string(forsta->index_name);
    tr_writef(" %s = ", name);

    if (forsta->iterator_assignment.node) {
        transpile_node(tr, forsta->iterator_assignment);
        tr_write("; ");
        transpile_node(tr, forsta->condition);
        tr_write("; ");
        if (forsta->iterator_update.node) transpile_node(tr, forsta->iterator_update);
        tr_write(")");
    } else {
        transpile_node(tr, forsta->min_expr);
        tr_writef("; %s < ", name);
        transpile_node(tr, forsta->max_expr);
        tr_writef("; %s++)", name);
    }

    if (forsta->statement.node) {
        tr_write(" ");
        transpile_node(tr, forsta->statement);
    }
}

static void transpile_node(C_Transpiler* tr, NodeRef p) {
char* operator = null;
switch (p.node->kind) {
    #define entry(op, str, label) case Node_##op: operator = str; goto label;
    entry(Plus,                "+",     binary)
    entry(Minus,               "-",     binary)
    entry(Mul,                 "*",     binary)
    entry(Div,                 "/",     binary)
    entry(Mod,                 "%",     binary)
    entry(Less,                "<",     binary)
    entry(Greater,             ">",     binary)
    entry(LessEquals,          "<=",    binary)
    entry(GreaterEquals,       ">=",    binary)
    entry(Equals,              "==",    binary)
    entry(NotEquals,           "!=",    binary)
    entry(BooleanAnd,          "&&",    binary)
    entry(BooleanOr,           "||",    binary)
    entry(Bitwise_And,         "&",     binary)
    entry(Bitwise_Or,          "|",     binary)
    entry(Bitwise_Xor,         "^",     binary)
    entry(Bitwise_Lshift,      "<<",    binary)
    entry(Bitwise_Rshift,      ">>",    binary)
    entry(Unary_PreIncrement,  "++",    unary)
    entry(Unary_PostIncrement, "++",    post_unary)
    entry(Unary_PreDecrement,  "--",    unary)
    entry(Unary_PostDecrement, "--",    post_unary)
    entry(Unary_Not,           "!",     unary)
    entry(Unary_BitwiseNot,    "~",     unary)
    entry(Unary_AddressOf,     "&",     unary)
    entry(Unary_ValueOf,       "*",     unary)
    entry(Unary_Negate,        "-",     unary)
    #undef entry

    binary:
        tr_write("(");
        transpile_node(tr, p.Binary->left);
        tr_writef(" %s ", operator);
        transpile_node(tr, p.Binary->right);
        tr_write(")");
        return;
    unary:
        tr_writef("%s", operator);
        transpile_node(tr, p.Unary->inner_expr);
        return;
    post_unary:
        transpile_node(tr, p.Unary->inner_expr);
        tr_writef("%s", operator);
        return;

    case Node_Literal_Integer: tr_writef("%llu", p.Literal->data.integer); return;
    case Node_Literal_Decimal: tr_writef("%lf", p.Literal->data.decimal); return;
    case Node_Literal_Char: {
        switch (p.Literal->data.character) { // TODO: escape sequences
            case '\n': tr_write("'\\n'"); return;
            case '\t': tr_write("'\\t'"); return;
            case '\\': tr_write("'\\\\'"); return;
            case '\'': tr_write("'\\''"); return;
        }
        tr_writef("'%c'", p.Literal->data.character);
    } return;
    case Node_Literal_String:  tr_writef("\"%s\"", get_string(p.Literal->data.string)); return;
    case Node_Literal_True:    tr_write("1"); return;
    case Node_Literal_False:   tr_write("0"); return;
    case Node_Literal_Null:    tr_write("0"); return;

    case Node_Variable: {
        if (p.Variable->ref.node) { // TODO: there is a null-check here. Which cases may ref be null? maybe put a comment here
            if (p.Variable->ref.node->kind == Node_Constant) { transpile_node(tr, p.Variable->ref.Constant->expr); return; }
            if (p.Variable->ref.node->kind == Node_Enum) return;
        }
        tr_write(get_string(p.Variable->name));
    } return;
    case Node_Alloc:          tr_write("malloc(sizeof("); transpileType(tr, p.Alloc->type); tr_write(")"); if (p.Alloc->size_expr.node) { tr_write(" * "); transpile_node(tr, p.Alloc->size_expr); } tr_write(")"); return;
    case Node_Ternary:        transpile_node(tr, p.Ternary->condition); tr_write(" ? "); transpile_node(tr, p.Ternary->then_expr); tr_write(" : "); transpile_node(tr, p.Ternary->else_expr); return;
    case Node_ProcCall:       transpile_proccall(tr, p.ProcCall); return;
    case Node_Deref: {

        if (p.Deref->datatype.kind == Typekind_Enum) {
            EnumEntry* entry = getEnumEntry(p.Deref->datatype._enum, p.Deref->name);
            if (entry) { // TODO: this introduces a bug if there is an enumentry that coincidentaly has the same name as a struct field, solution: use deref->ref
                tr_writef("%llu", entry->value);
                return;
            }
        }

        transpile_node(tr, p.Deref->expr);
        if (p.Deref->expr.expr->datatype.numPointers) tr_write("->");
        else tr_write(".");
        tr_write(get_string(p.Deref->name));
    } return;
    case Node_Indexing: {
        if (p.Indexing->indexed.expr->datatype.kind == Typekind_Array) {
            tr_write("((");
            Datatype elm_type = p.Indexing->indexed.expr->datatype.array_typenode->array.element_type->solvedstate;
            elm_type.numPointers++;
            transpileDatatype(tr, elm_type);
            tr_write(")");
            transpile_node(tr, p.Indexing->indexed);
            tr_write(".data)");
        } else {
            transpile_node(tr, p.Indexing->indexed);
        }

        tr_write("["); transpile_node(tr, p.Indexing->index); tr_write("]");
    } return;
    case Node_Cast:           tr_write("("); transpileType(tr, p.Cast->new_type); tr_write(")"); transpile_node(tr, p.Cast->expr); return;
    case Node_Sizeof:         tr_write("sizeof("); transpileType(tr, p.Sizeof->type); tr_write(")"); return;
    case Node_Parenthesized:  if (is_binary_expr(p.Parenthesized->inner_expr)) transpile_node(tr, p.Parenthesized->inner_expr);
                              else { tr_write("("); transpile_node(tr, p.Parenthesized->inner_expr); tr_write(")"); } return;
    case Node_Compound: {
        if (p.Compound->datatype.kind != Typekind_Invalid) {
            tr_write("("); transpileDatatype(tr, p.Compound->datatype); tr_write(") ");
            if (p.Compound->datatype.kind == Typekind_Array) {
                tr_writef("{ .length = %d, .data = (", p.Compound->elements ? list_length(p.Compound->elements) : 0);
                transpileDatatype(tr, p.Compound->datatype.array_typenode->array.element_type->solvedstate);
                tr_write("[])");
            }
        }

        tr_write("{");
        if (p.Compound->elements) {
            foreach (elm, p.Compound->elements) {
                if (elm->name) tr_writef(".%s = ", get_string(elm->name));
                transpile_node(tr, elm->expr);
                tr_write(", ");
            }
            tr->sb.length -= 2;
        } else tr_write("0");
        tr_write("}");

        if (p.Compound->datatype.kind == Typekind_Array) tr_write("}");
    } return;

    case Node_Declaration:  transpile_declaration(tr, p.Declaration); return;
    case Node_Constant:     tr_write("/* local constant */"); return;
    case Node_Typedef:      transpile_typedef(tr, p.Typedef); return;
    case Node_Procedure:    tr_write("/* local procedure */"); return;
    case Node_Argument:     return;
    case Node_Struct:       tr_writef("typedef struct %s %s;", get_string(p.Struct->name), get_string(p.Struct->name)); newline(tr); transpile_struct(tr, p.Struct); return;
    case Node_Enum:         tr_writef("typedef uint32 %s", get_string(p.Enum->name)); return;
    case Node_EnumEntry:    return;

    static const char* assignment_operator[] = {
        [Tok_Assign]       = "=",
        [Tok_PlusAssign]   = "+=",
        [Tok_MinusAssign]  = "-=",
        [Tok_MulAssign]    = "*=",
        [Tok_DivAssign]    = "/=",
        [Tok_BitAndAssign] = "&=",
        [Tok_BitOrAssign]  = "|=",
        [Tok_BitXorAssign] = "^=",
    };

    case Node_Assignment:   transpile_node(tr, p.Assignment->dst_expr); tr_writef(" %s ", assignment_operator[p.Assignment->operator]); transpile_node(tr, p.Assignment->src_expr); return;
    case Node_Scope:        transpile_scope(tr, p.Scope); return;

    case Node_IfStmt:
        tr_write("if ");
        transpile_condition_expr(tr, p.IfStmt->condition);
        if (p.IfStmt->then_statement.node) { tr_write(" "); transpile_node(tr, p.IfStmt->then_statement); }
        if (p.IfStmt->else_statement.node) { transpile_semicolon_after_node(tr, p.IfStmt->then_statement); tr_write(" else "); transpile_node(tr, p.IfStmt->else_statement); }
        return;

    case Node_WhileStmt:
        tr_write("while "); transpile_condition_expr(tr, p.WhileStmt->condition);
        if (p.WhileStmt->statement.node) { tr_write(" "); transpile_node(tr, p.WhileStmt->statement); }
        return;

    case Node_ForStmt:          transpile_for_loop(tr, p.ForStmt); return;
    case Node_SwitchStmt:       tr_write("switch "); transpile_condition_expr(tr, p.SwitchStmt->expr); tr_write(" "); transpile_scope(tr, p.SwitchStmt->scope); return;
    case Node_ContinueStmt:     tr_write("continue"); return;
    case Node_BreakStmt:        tr_write("break"); return;
    case Node_ReturnStmt:       tr_write("return"); if (p.ReturnStmt->expr.node) { tr_write(" "); transpile_node(tr, p.ReturnStmt->expr); } return;
    case Node_GotoStmt:         tr_writef("goto %s", get_string(p.GotoStmt->label)); return;
    case Node_LabelStmt:        tr_writef("%s:", get_string(p.LabelStmt->label)); return;
    case Node_CaseLabelStmt:    tr_write("case "); transpile_node(tr, p.CaseLabelStmt->expr); tr_write(":"); return;
    case Node_DefaultLabelStmt: tr_write("default:"); return;

    case Node_Type_MustInfer: return;
    case Node_Type_Basic: return;
    case Node_Type_Procedure: return;
    case Node_Type_Array: return;
    case Node_Type_Fixed_Array: return;
    case Node_Type_Dynamic_Array: return;

    case Node_Invalid: return;
    case Nodekind_Count: return;
}}

static u32 countStructDependencies(Struct* stru) {
    u32 deps = 0;
    u32 fieldsLen = list_length(stru->fields);
    for (u32 f = 0; f < fieldsLen; f++) {
        Datatype datatype = stru->fields[f].type->solvedstate;
        if (datatype.numPointers) continue;
        if (datatype.kind != Typekind_Struct) continue;

        deps++;
        deps += countStructDependencies(datatype.stru);
    }

    return deps;
}

void transpile(Codebase* codebase) {
    // TODO: use a higer initial capacity for the string builder

    C_Transpiler transpiler = {0}, *tr = &transpiler;
    transpiler.sb = sbCreate();
    list_init(transpiler.static_decls);


    // tr_write("#include <stdlib.h>\n"); // malloc
    // tr_write("#include <stdio.h>\n"); // printf
    // tr_write("#define true 1\n#define false 0\n");

    tr_write("\n// Basics\n");
    tr_write("typedef signed char int8;\n");
    tr_write("typedef signed short int16;\n");
    tr_write("typedef signed int int32;\n");
    tr_write("typedef signed long long int64;\n");
    tr_write("typedef unsigned char uint8;\n");
    tr_write("typedef unsigned short uint16;\n");
    tr_write("typedef unsigned int uint32;\n");
    tr_write("typedef unsigned long long uint64;\n");
    tr_write("typedef float float32;\n");
    tr_write("typedef double float64;\n");
    tr_write("int printf(const char* format, ...);\n");
    tr_write("typedef struct Array { void* data; uint32 length; } Array;\n");


    tr_write("\n// Structs forward declarations\n");
    u32 structs_count = list_length(codebase->structs);
    for (u32 i = 0; i < structs_count; i++) {
        Struct* s = codebase->structs[i];
        char* name = get_string(s->name);
        tr_writef("typedef struct %s %s;\n", name, name);
    }

    tr_write("\n// Enums\n");
    foreach (enu, codebase->enums) {
        Enum* en = *enu;
        tr_writef("typedef uint32 %s;\n", get_string(en->name));
    }


    tr_write("\n// Type aliases\n");
    foreach (type_def, codebase->type_defs) {
        transpile_typedef(tr, *type_def);
        tr_write("\n");
    }


    {
        tr_write("\n// Structs\n");
        for (u32 i = 0; i < structs_count; i++) {
            codebase->structs[i]->deps = countStructDependencies(codebase->structs[i]);
        }

        u32 dep = 0;
        u32 transpiled = 0;
        while (transpiled < structs_count) {
            for (u32 i = 0; i < structs_count; i++) {
                Struct* stru = codebase->structs[i];
                if (stru->deps == dep) {
                    transpile_struct(tr, stru);
                    tr_write(";\n");
                    transpiled++;
                }
            }
            dep++;
        }
    }

    tr_write("\n// Forward declarations\n");
    u32 procs_count = list_length(codebase->procedures);
    for (u32 i = 0; i < procs_count; i++) {
        transpile_proc_signature(tr, codebase->procedures[i]);
        tr_write(";\n");
    }

    tr_write("\n// Declarations\n");
    foreach (global_var, codebase->global_vars) {
        Declaration* decl = *global_var;

        if (decl->expr.node && !isCompiletimeExpression(decl->expr)) {
            list_add(tr->static_decls, decl);
            continue;
        }

        tr_write("static ");
        _transpileDatatype(tr, decl->type->solvedstate, decl->name);
        if (decl->expr.node) {
            tr_write(" = ");
            transpile_node(tr, decl->expr);
        }
        tr_write(";\n");
    }

    {
        StringBuilder temp = tr->sb;
        tr->sb = sbCreate(); // I am such a dirty trickster

        tr_write("\n// Implementations\n");
        for (u32 i = 0; i < procs_count; i++) {
            Procedure* proc = codebase->procedures[i];
            if (proc->scope == null) continue;
            transpile_procedure(tr, proc);
        }

        StringBuilder impl_sb = tr->sb;
        tr->sb = temp;

        foreach (static_decl, tr->static_decls) {
            Declaration* decl = *static_decl;
            tr_write("static ");
            _transpileDatatype(tr, decl->type->solvedstate, decl->name);
            if (decl->expr.node && isCompiletimeExpression(decl->expr)) {
                tr_write(" = ");
                transpile_node(tr, decl->expr);
            }
            tr_write(";\n");
        }

        tr_write(impl_sb.content);
        sbDestroy(&impl_sb);
    }

    tr_write("static void __static_init() {");
    tr->tabing++;
    foreach (static_decl, tr->static_decls) {
        Declaration* decl = *static_decl;
        if (decl->expr.node && isCompiletimeExpression(decl->expr)) continue;
        newline(tr);
        tr_write(get_string(decl->name));
        tr_write(" = ");
        transpile_node(tr, decl->expr);
        tr_write(";");
    }
    tr->tabing--;
    newline(tr);
    tr_write("}\n");

    char* c_main_code =
        "int main(int argc, char** argv) {\n"
        "    __static_init();\n"
        "    __main();\n"
        "    return 0;\n"
        "}";

    tr_write(c_main_code);


    FILE* file;
    if ( !fopen_s(&file, "output.g.c", "w") ) {
        fprintf(file, "%s", tr->sb.content);
        fclose(file);
    } else {
        printf("Could not write to output.g.c\n");
    }

    sbDestroy(&tr->sb);
}