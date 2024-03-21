

static void transpileScope(Scope* scope);
static void transpileExpression(Expression* expr);
static void transpileStatement(Statement* statement);
static void transpileProcedure(Procedure* proc);


typedef struct C_Transpiler {
    Declaration** static_decls; // list
    StringBuilder sb;
    u32 tabing;
} C_Transpiler;

#define tr_write(c_str) sbAppend(&tr->sb, c_str)
#define tr_writef(format, ...) sb_append_format(&tr->sb, format, __VA_ARGS__)

static inline void newline(C_Transpiler* tr) {
    tr_writef("\n");
    u32 t = tr->tabing;
    while (t--) tr_writef("    ");
}


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

static void _transpileDatatype(Datatype type, Identifier name);
static void transpileDatatype(Datatype type);
static void transpileType(Type* type) { transpileDatatype(type->solvedstate); }

static void transpileProcSigStart(Type* proc_type) {
    if (proc_type->procedure.return_type->solvedstate.kind == Typekind_Procedure) {
        transpileProcSigStart(proc_type->procedure.return_type);
    } else {
        transpileDatatype(proc_type->procedure.return_type->solvedstate);
        tr_write(" ");
    }

    tr_write("(");
    u32 np = proc_type->solvedstate.numPointers;
    while (np-- > 0) tr_write("*");
}

static void transpileProcArguments(Type* proc_type) {
    tr_write("(");
    Type* arg = proc_type->procedure.first_argument;
    while (arg) {
        transpileDatatype(arg->solvedstate);
        if (arg->next) tr_write(", ");
        arg = arg->next;
    }
    tr_write(")");
}

static void transpileProcSigEnd(Type* proc_type) {
    tr_write(")");
    transpileProcArguments(proc_type);

    while (proc_type->procedure.return_type->solvedstate.kind == Typekind_Procedure) {
        tr_write(")");
        proc_type = proc_type->procedure.return_type;
        transpileProcArguments(proc_type);
    }
}

static void transpileTypeStart(Datatype type) {

    if (type.kind == Typekind_Array) {
        tr_write("Array");

        u32 np = type.numPointers;
        while (np-- > 0) tr_write("*");
        return;
    }

    if ((type.kind == Typekind_Fixed_Array) ||
        (type.kind == Typekind_Dynamic_Array)) {

        transpileTypeStart(type.array_typenode->array.element_type->solvedstate);
        tr_write("*");
        return;
    }


    if (type.kind == Typekind_Procedure) {
        transpileProcSigStart(type.proc_ptr_typenode);
    } else {
        tr_write(getTypeCname(type));
        u32 np = type.numPointers;
        while (np-- > 0) tr_write("*");
        //tr_write(" ");
    }
}

static void transpileTypeEnd(Datatype type) {
    if (type.kind == Typekind_Procedure) transpileProcSigEnd(type.proc_ptr_typenode);
}

static void transpileDatatype(Datatype type) {
    transpileTypeStart(type);
    transpileTypeEnd(type);
}

static void _transpileDatatype(Datatype type, Identifier name) {
    transpileTypeStart(type);
    if (name && type.kind != Typekind_Procedure) tr_write(" ");
    tr_write(get_string(name));
    transpileTypeEnd(type);
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

static void transpileFuncCall(ProcCallExpression* call) {

    transpileExpression(call->proc_expr);
    if (call->proc && call->proc->overload) sbAppendSpan(sb, numberToString((u64)call->proc->overload));

    tr_write("(");
    if (call->args) {
        transpileExpression(call->args[0]);

        u32 len = list_length(call->args);
        for (u32 i = 1; i < len; i++) {
            tr_write(", ");
            transpileExpression(call->args[i]);
        }
    }
    tr_write(")");
}


static void transpilePrintFormat(Datatype type) {
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
            transpilePrintFormat(field->type->solvedstate);
            tr_write(", ");
        }
        sb->length -= 2;
        tr_write("}");
        return;
    }

    tr_write("<print_error>");
}

static void transpilePrint(Expression** args) {
    tr_write("printf(\"");
    {
        foreach (item, args) {
            Expression* arg = *item;
            transpilePrintFormat(arg->datatype);
        }
    }
    tr_write("\"");


    foreach (item, args) {
        Expression* arg = *item;
        tr_write(", ");
        if (arg->datatype.kind == Typekind_Struct && arg->datatype.numPointers == 0) {
            foreach (field, arg->datatype.stru->fields) {

                if (field->type->solvedstate.kind == Typekind_Struct && field->type->solvedstate.numPointers == 0) {
                    // TODO: print struct inside struct
                }

                transpileExpression(arg);
                tr_write(".");
                tr_write(get_string(field->name));
                tr_write(", ");
            }
            sb->length -= 2;
        } else {
            if (arg->expressionType == ExprType_Sizeof) tr_write("(uint32)");
            transpileExpression(arg);
        }
    }

    tr_write(")");
}

static void transpileExpression(Expression* expr) {

    switch (expr->expressionType) {

        { // Binary & Unary Expressions

            case ExprType_Unary_PostIncrement: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                transpileExpression(unary->expr);
                tr_write("++");
            } break;
            case ExprType_Unary_PostDecrement: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                transpileExpression(unary->expr);
                tr_write("--");
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
                tr_write(operator);
                transpileExpression(unary->expr);
            } break;

            binary: {
                BinaryExpression* bop = (BinaryExpression*)expr;
                tr_write("(");
                transpileExpression(bop->left);
                tr_write(operator);
                transpileExpression(bop->right);
                tr_write(")");
            } break;
        }

        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;

            if (var->ref) {
                StatementType ref_type = var->ref->statementType;
                if (ref_type == Statement_Constant) {
                    transpileExpression(((Declaration*)var->ref)->expr);
                    break;
                }

                if (ref_type == Statement_Enum) break;
            }

            tr_write(get_string(var->name));
        } break;

        case ExprType_Deref: {
            DerefExpression* deref = (DerefExpression*)expr;

            if (deref->base.datatype.kind == Typekind_Enum) {
                EnumEntry* entry = getEnumEntry(deref->base.datatype._enum, deref->name);
                if (entry) { // TODO: this introduces a bug if there is an enumentry that coincidentaly has the same name as a struct field, solution: use deref->ref
                    sbAppendSpan(sb, numberToString(entry->value));
                    break;
                }
            }

            transpileExpression(deref->expr);

            if (deref->expr->datatype.numPointers) tr_write("->");
            else tr_write(".");



            // I belive this had to do with contextual inclusion of fields
            // if (deref->expr->datatype.kind == Typekind_Struct) {
            //     Struct* stru = deref->expr->datatype.stru;
            //     foreach (field, stru->fields) {
            //         if (field->name == deref->name) goto done;
                    
            //     }
            // }
            // done:

            tr_write(get_string(deref->name));
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;

            if (ind->indexed->datatype.kind == Typekind_Array) {
                tr_write("((");
                Datatype elm_type = ind->indexed->datatype.array_typenode->array.element_type->solvedstate;
                elm_type.numPointers++;
                transpileDatatype(elm_type);
                tr_write(")");
                transpileExpression(ind->indexed);
                tr_write(".data)");
            } else {
                transpileExpression(ind->indexed);
            }

            tr_write("[");
            transpileExpression(ind->index);
            tr_write("]");
        } break;

        case ExprType_Literal_Null: {
            tr_write("0");
        } break;

        case ExprType_Literal_String: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            tr_write("\"");
            tr_write(get_string(lit->data.string));
            tr_write("\"");
        } break;

        case ExprType_Literal_Char: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendChar(sb, '\'');

            switch (lit->data.character) {
                case '\n': tr_write("\\n"); break;
                case '\t': tr_write("\\t"); break;
                case '\\': tr_write("\\\\"); break;
                case '\'': tr_write("\\'"); break;

                default:
                    sbAppendChar(sb, lit->data.character);
                    break;
            }

            sbAppendChar(sb, '\'');
        } break;

        case ExprType_Literal_True: tr_write("1"); break;
        case ExprType_Literal_False: tr_write("0"); break;

        case ExprType_Literal_Integer: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendSpan(sb, numberToString(lit->data.integer));
        } break;

        case ExprType_Literal_Decimal: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            char temp_buffer[24];
            u32 i = snprintf(temp_buffer, 23, "%f", lit->data.decimal);
            temp_buffer[i] = '\0';
            tr_write(temp_buffer);
        } break;

        case ExprType_Alloc: {
            AllocExpression* allocExpr = (AllocExpression*)expr;
            tr_write("malloc(sizeof(");
            transpileType(allocExpr->type);
            tr_write(")");
            if (allocExpr->sizeExpr) {
                tr_write(" * ");
                transpileExpression(allocExpr->sizeExpr);
            }
            tr_write(")");
        } break;

        case ExprType_Sizeof: {
            SizeofExpression* sof = (SizeofExpression*)expr;
            tr_write("sizeof(");
            transpileType(sof->type);
            tr_write(")");
        } break;

        case ExprType_Cast: {
            CastExpression* cast = (CastExpression*)expr;
            tr_write("(");
            transpileType(cast->castToType);
            tr_write(")");
            transpileExpression(cast->expr);
        } break;

        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            transpileExpression(ter->condition);
            tr_write(" ? ");
            transpileExpression(ter->thenExpr);
            tr_write(" : ");
            transpileExpression(ter->elseExpr);
        } break;

        case ExprType_ProcCall: {
            ProcCall* fc = (ProcCall*)expr;

            if (fc->proc_expr->expressionType == ExprType_Variable) {
                VariableExpression* var = (VariableExpression*)fc->proc_expr;
                if (var->name == builtin_string_print) {
                    transpilePrint(fc->args);
                    break;
                }
            }

            transpileFuncCall(fc);
        } break;

        case ExprType_Parenthesized: {
            ParenthesizedExpression* p = (ParenthesizedExpression*)expr;
            if (isBinaryExpression(p->innerExpr)) {
                transpileExpression(p->innerExpr);
            } else {
                tr_write("(");
                transpileExpression(p->innerExpr);
                tr_write(")");
            }
        } break;

        case ExprType_Compound: {
            CompoundExpression* com = (CompoundExpression*)expr;

            if (com->base.datatype.kind != Typekind_Invalid) {
                tr_write("(");
                transpileDatatype(com->base.datatype);
                tr_write(") ");

                if (com->base.datatype.kind == Typekind_Array) {
                    tr_write("{ .length = ");
                    u32 len = com->elements ? list_length(com->elements) : 0;
                    sbAppendSpan(sb, numberToString(len));
                    tr_write(", .data = (");
                    transpileDatatype(com->base.datatype.array_typenode->array.element_type->solvedstate);
                    tr_write("[])");
                }
            }

            tr_write("{");
            if (com->elements) {
                foreach (elm, com->elements) {
                    if (elm->name) {
                        tr_write(".");
                        tr_write(get_string(elm->name));
                        tr_write(" = ");
                    }
                    transpileExpression(elm->expr);
                    tr_write(", ");
                }
                sb->length -= 2;
            } else {
                tr_write("0");
            }
            tr_write("}");

            if (com->base.datatype.kind == Typekind_Array) tr_write("}");
        } break;
    }

}


static void transpile_condition_expr(C_Transpiler* tr, NodeRef ref) {
    if (ref.node->kind == Node_Parenthesized || is_binary_expr(ref)) transpile_node(tr, ref);
    else { tr_write("("); transpile_node(tr, ref); tr_write(")"); }
}


static void transpileStruct(Struct* stru) {
    tr_writef("struct %s {", get_string(stru->name));

    tr->tabing++;

    foreach (field, stru->fields) {
        newline();
        _transpileDatatype(field->type->solvedstate, field->name);
        tr_write(";");
    }

    tr->tabing--;
    newline();

    tr_write("};");
}

static void transpileTypedef(Typedef* def) {
    tr_write("typedef ");
    if (def->type) {
        _transpileDatatype(def->type->solvedstate, def->name);
    } else {
        tr_write("struct ");
        char* name = get_string(def->name);
        tr_write(name);
        tr_write(" ");
        tr_write(name);
    }
    tr_write(";");
}

static void transpileVarDecl(Declaration* decl) {

    if (decl->is_static) {
        list_add(static_decls, decl);
        tr_write("// static decl");
        return;
    }

    _transpileDatatype(decl->type->solvedstate, decl->name);
    if (decl->expr) {
        tr_write(" = ");
        transpileExpression(decl->expr);
    }

    tr_write(";");
}

static void transpileStatement(Statement* statement) {
    switch (statement->statementType) {
        case Statement_Declaration: {
            Declaration* decl = (Declaration*)statement;
            transpileVarDecl(decl);
        } break;

        case Statement_Struct: {
            Struct* stru = (Struct*)statement;
            tr_write("typedef struct ");
            tr_write(get_string(stru->name));
            tr_write(" ");
            tr_write(get_string(stru->name));
            tr_write(";");
            newline();
            transpileStruct(stru);
        } break;

        case Statement_Enum: {
            Enum* en = (Enum*)statement;
            tr_write("typedef uint32 ");
            tr_write(get_string(en->name));
            tr_write(";");
        } break;

        case Statement_Typedef: {
            transpileTypedef((Typedef*)statement);
        } break;

        case Statement_Constant: {
            tr_write("/* local constant */");
        } break;

        case Statement_Assignment: {
            Assignment* ass = (Assignment*)statement;
            transpileExpression(ass->assigneeExpr);

            switch (ass->assignmentOper) {
                case Tok_Assign: tr_write(" = "); break;
                case Tok_PlusAssign: tr_write(" += "); break;
                case Tok_MinusAssign: tr_write(" -= "); break;
                case Tok_MulAssign: tr_write(" *= "); break;
                case Tok_DivAssign: tr_write(" /= "); break;
                case Tok_BitAndAssign: tr_write(" &= "); break;
                case Tok_BitOrAssign: tr_write(" |= "); break;
                case Tok_BitXorAssign: tr_write(" ^= "); break;
                default: break;
            }

            transpileExpression(ass->expr);
            tr_write(";");
        } break;


        case Statement_For: {
            ForStatement* forsta = (ForStatement*)statement;
            char* name = get_string(forsta->index_name);

            tr_write("for (");

            if (forsta->index_type) transpileType(forsta->index_type);
            else transpileDatatype(default_for_loop_numeric_type);

            tr_write(" ");
            tr_write(name);
            tr_write(" = ");

            if (forsta->iterator_assignment) {

                transpileExpression(forsta->iterator_assignment);
                tr_write("; ");
                transpileExpression(forsta->condition);
                tr_write("; ");
                if (forsta->iterator_update) transpileExpression(forsta->iterator_update);
                tr_write(")");

            } else {
                transpileExpression(forsta->min_expr);
                tr_write("; ");

                tr_write(name);
                tr_write(" < ");
                transpileExpression(forsta->max_expr);
                tr_write("; ");

                tr_write(name);
                tr_write("++)");
            }

            if (forsta->statement) {
                tr_write(" ");
                transpileStatement(forsta->statement);
            } else tr_write(";");
        } break;

        case Statement_Procedure: {
            tr_write("// local procedure");
        } break;




        case Statement_Argument:
        case Statement_EnumEntry: {
            // Unreachable code
        } break;
    }
}

static void transpileScope(Scope* scope) {
    tr_write("{");
    tr->tabing++;

    u32 len = list_length(scope->statements);
    for (u32 i = 0; i < len; i++) {
        newline();
        transpileStatement(scope->statements[i]);
    }

    tr->tabing--;
    newline();

    tr_write("}");
}

static void transpile_scope(C_Transpiler* tr, Scope* scope) {
    tr_write("{"); tr->tabing++;

    foreach (refp, scope->statements) {
        newline(tr);
        transpile_node(tr, *refp);
    }

    tr->tabing--; newline(tr);
    tr_write("}");
}

static void transpileFunctionSignature(Procedure* proc) {


    if (proc->name != builtin_string_main && proc->scope) tr_write("static ");
    transpileType(proc->returnType);
    tr_write(" ");

    if (proc->name == builtin_string_main) tr_write("__main");
    else tr_write(get_string(proc->name));

    if (proc->overload) sbAppendSpan(sb, numberToString((u64)proc->overload));

    tr_write("(");
    if (proc->arguments) {
        foreach (arg, proc->arguments) {
            _transpileDatatype(arg->type->solvedstate, arg->name);
            tr_write(", ");
        }
        sb->length -= 2;
    }
    tr_write(")");
}

static void transpileLocalProcedures(Statement* statement) {
    if (!statement) return;
    if (statement->statementType != Statement_Scope) return;
    Scope* scope = (Scope*)statement;
    foreach (s, scope->statements) {
        Statement* sta = *s;
        switch (sta->statementType) {
            case Statement_Procedure: transpileProcedure((Procedure*)sta); break;
            case Statement_Scope:     transpileLocalProcedures(sta); break;
            case Statement_If:        transpileLocalProcedures(((IfStatement*)sta)->then_statement);
                                      transpileLocalProcedures(((IfStatement*)sta)->else_statement); break;
            case Statement_While:     transpileLocalProcedures(((WhileStatement*)sta)->statement); break;
            case Statement_For:       transpileLocalProcedures(((ForStatement*)sta)->statement); break;
            default: break;
        }
    }
}

static void transpileProcedure(Procedure* proc) {

    transpileLocalProcedures((Statement*)proc->scope);

    transpileFunctionSignature(proc);
    tr_write(" ");
    transpileScope(proc->scope);
    newline();
}



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


static void transpile_node(C_Transpiler* tr, NodeRef p) {
char* operator = null;
switch (p.node->kind) {
    #define foreach_c_operator(X)\
        X(Plus, "+", binary) X(Minus, "-", binary) X(Mul, "*", binary) X(Div, "/", binary) X(Mod, "%", binary)\
        X(Less, "<", binary) X(Greater, ">", binary) X(LessEquals, "<=", binary) X(GreaterEquals, ">=", binary)\
        X(Equals, "==", binary) X(NotEquals, "!=", binary) X(BooleanAnd, "&&", binary) X(BooleanOr, "||", binary)\
        X(Bitwise_And, "&", binary) X(Bitwise_Or, "|", binary) X(Bitwise_Xor, "^", binary) X(Bitwise_Lshift, "<<", binary) X(Bitwise_Rshift, ">>", binary)\
        X(Unary_PreIncrement, "++", unary) X(Unary_PostIncrement, "++", post_unary)\
        X(Unary_PreDecrement, "--", unary) X(Unary_PostDecrement, "--", post_unary)\
        X(Unary_Not, "!", unary) X(Unary_BitwiseNot, "~", unary) X(Unary_AddressOf, "*", unary)\
        X(Unary_ValueOf, "@", unary) X(Unary_Negate, "-", unary)\

    #define entry(op, str, label) case Node_##op: operator = str; goto label;
    foreach_c_operator(entry)

    #undef entry
    #undef foreach_c_operator

    binary:
        tr_write("(");
        transpile_node(tr, p.Binary->left);
        tr_write(get_c_operator_symbol(p.node->kind));
        transpile_node(tr, p.Binary->right);
        tr_write(")");
        return;
    unary:
        tr_write(get_c_operator_symbol(p.node->kind));
        transpile_node(tr, p.Unary->inner_expr);
        return;
    post_unary:
        transpile_node(tr, p.Unary->inner_expr);
        tr_write(get_c_operator_symbol(p.node->kind));
        return;

// case '\n': tr_write("\\n"); break;
// case '\t': tr_write("\\t"); break;
// case '\\': tr_write("\\\\"); break;
// case '\'': tr_write("\\'"); break;
    case Node_Literal_Integer: tr_writef("%llu", p.Literal->data.integer); return;
    case Node_Literal_Decimal: tr_writef("%lf", p.Literal->data.decimal); return;
    case Node_Literal_Char:    tr_writef("'%c'", p.Literal->data.character); return; // TODO: escape sequences
    case Node_Literal_String:  tr_writef("\"%s\"", get_string(p.Literal->data.string)); return;
    case Node_Literal_True:    tr_write("1"); return;
    case Node_Literal_False:   tr_write("0"); return;
    case Node_Literal_Null:    tr_write("0"); return;

    case Node_Variable: return;
    case Node_Alloc: return;
    case Node_Ternary: return;
    case Node_ProcCall: return;
    case Node_Deref: return;
    case Node_Indexing: return;
    case Node_Cast: return;
    case Node_Sizeof: return;
    case Node_Parenthesized: return;
    case Node_Compound: return;

/*

*/

    case Node_Declaration:      return;
    case Node_Constant:         return;
    case Node_Typedef:          return;
    case Node_Procedure:        return;
    case Node_Argument:         return;
    case Node_Struct:           return;
    case Node_Enum:             return;
    case Node_EnumEntry:        return;
    case Node_Assignment:       return;
    case Node_Scope:            transpile_scope(tr, p.Scope); return;

    case Node_IfStmt:
        tr_write("if ");
        transpile_condition_expr(tr, p.IfStmt->condition);
        if (p.IfStmt->then_statement.node) { tr_write(" "); transpile_node(tr, p.IfStmt->then_statement); } else tr_write(";");
        if (p.IfStmt->else_statement.node) { tr_write(" else "); transpile_node(tr, p.IfStmt->else_statement); }
        return;

    case Node_WhileStmt:
        tr_write("while "); transpile_condition_expr(tr, p.WhileStmt->condition);
        if (p.WhileStmt->statement.node) { tr_write(" "); transpile_node(tr, p.WhileStmt->statement); }
        else tr_write(";");
        return;

    case Node_ForStmt:          return;
    case Node_SwitchStmt:       tr_write("switch "); transpile_condition_expr(tr, p.SwitchStmt->expr); tr_write(" "); transpile_scope(tr, p.SwitchStmt->scope); return;
    case Node_ContinueStmt:     tr_write("continue;"); return;
    case Node_BreakStmt:        tr_write("break;"); return;
    case Node_ReturnStmt:       tr_write("return"); if (p.ReturnStmt->expr.node) { tr_write(" "); transpile_node(tr, p.ReturnStmt->expr); } tr_write(";"); return;
    case Node_GotoStmt:         tr_writef("goto %s;", get_string(p.GotoStmt->label)); return;
    case Node_LabelStmt:        tr_writef("%s:", get_string(p.LabelStmt->label)); return;
    case Node_CaseLabelStmt:    tr_write("case: "); transpile_node(tr, p.CaseLabelStmt->expr); tr_write(":"); return;
    case Node_DefaultLabelStmt: tr_write("default:"); return;

    case Node_Type_MustInfer: return;
    case Node_Type_Basic: return;
    case Node_Type_Procedure: return;
    case Node_Type_Array: return;
    case Node_Type_Fixed_Array: return;
    case Node_Type_Dynamic_Array: return;
}}

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
        transpileTypedef(*type_def);
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
                    transpileStruct(stru);
                    tr_write("\n");
                    transpiled++;
                }
            }
            dep++;
        }
    }

    tr_write("\n// Forward declarations\n");
    u32 procs_count = list_length(codebase->procedures);
    for (u32 i = 0; i < procs_count; i++) {
        transpileFunctionSignature(codebase->procedures[i]);
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
        _transpileDatatype(decl->type->solvedstate, decl->name);
        if (decl->expr.node) {
            tr_write(" = ");
            transpileExpression(decl->expr);
        }
        tr_write(";\n");
    }

    {
        StringBuilder impl_sb = sbCreate(); // I am such a dirty trickster
        StringBuilder* temp = sb;
        sb = &impl_sb;

        tr_write("\n// Implementations\n");
        for (u32 i = 0; i < procs_count; i++) {
            Procedure* proc = codebase->procedures[i];
            if (proc->scope == null) continue;
            transpileProcedure(proc);
        }

        sb = temp;

        foreach (static_decl, static_decls) {
            Declaration* decl = *static_decl;
            tr_write("static ");
            _transpileDatatype(decl->type->solvedstate, decl->name);
            if (decl->expr && isCompiletimeExpression(decl->expr)) {
                tr_write(" = ");
                transpileExpression(decl->expr);
            }
            tr_write(";\n");
        }

        tr_write(impl_sb.content);
        sbDestroy(&impl_sb);
    }

    tr_write("static void __static_init() {");
    tr->tabing++;
    foreach (static_decl, static_decls) {
        Declaration* decl = *static_decl;
        if (decl->expr && isCompiletimeExpression(decl->expr)) continue;
        newline();
        tr_write(get_string(decl->name));
        tr_write(" = ");
        transpileExpression(decl->expr);
        tr_write(";");
    }
    tr->tabing--;
    newline();
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
        fprintf(file, "%s", sb->content);
        fclose(file);
    } else {
        printf("Could not write to output.g.c\n");
    }

    sbDestroy(sb);
}