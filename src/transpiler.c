

static void transpileScope(Scope* scope);
static void transpileExpression(Expression* expr);
static void transpileStatement(Statement* statement);
static void transpileProcedure(Procedure* proc);

static Declaration** static_decls; // list

static StringBuilder* sb;

static u32 tabing = 0;
static inline void newline() {

    // sbAppend(sb, " ");
    // return;

    sbAppend(sb, "\n");
    u32 t = tabing;
    while (t--) sbAppend(sb, "    ");
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
        sbAppend(sb, " ");
    }

    sbAppend(sb, "(");
    u32 np = proc_type->solvedstate.numPointers;
    while (np-- > 0) sbAppend(sb, "*");
}

static void transpileProcArguments(Type* proc_type) {
    sbAppend(sb, "(");
    Type* arg = proc_type->procedure.first_argument;
    while (arg) {
        transpileDatatype(arg->solvedstate);
        if (arg->next) sbAppend(sb, ", ");
        arg = arg->next;
    }
    sbAppend(sb, ")");
}

static void transpileProcSigEnd(Type* proc_type) {
    sbAppend(sb, ")");
    transpileProcArguments(proc_type);

    while (proc_type->procedure.return_type->solvedstate.kind == Typekind_Procedure) {
        sbAppend(sb, ")");
        proc_type = proc_type->procedure.return_type;
        transpileProcArguments(proc_type);
    }
}

static void transpileTypeStart(Datatype type) {

    if (type.kind == Typekind_Array) {
        sbAppend(sb, "Array");

        u32 np = type.numPointers;
        while (np-- > 0) sbAppend(sb, "*");
        return;
    }

    if ((type.kind == Typekind_Fixed_Array) ||
        (type.kind == Typekind_Dynamic_Array)) {

        transpileTypeStart(type.array_typenode->array.element_type->solvedstate);
        sbAppend(sb, "*");
        return;
    }


    if (type.kind == Typekind_Procedure) {
        transpileProcSigStart(type.proc_ptr_typenode);
    } else {
        sbAppend(sb, getTypeCname(type));
        u32 np = type.numPointers;
        while (np-- > 0) sbAppend(sb, "*");
        //sbAppend(sb, " ");
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
    if (name && type.kind != Typekind_Procedure) sbAppend(sb, " ");
    sbAppend(sb, get_string(name));
    transpileTypeEnd(type);
}



// static void transpile_declarator(Datatype datatype, Identifier name) {
//     if (datatype.kind == Typekind_Procedure) {

//         sbAppend(sb, "(");
//         transpile_declarator(datatype.proc_ptr_typenode->procedure.return_type, name);
//         sbAppend(sb, ")");

//         sbAppend(sb, "(");
//         // args
//         sbAppend(sb, ")");
//     } else {
//         u32 np = datatype.numPointers;
//         while (np-- > 0) sbAppend(sb, "*");
//         sbAppend(sb, get_string(name));
//     }
// }

// static void transpile_procptr(Type* proc_type) {
//     if (proc_type->procedure.return_type->node_type == TypeNode_Procedure) {
//         transpile_procptr(proc_type->procedure.return_type);
//     }

//     sbAppend(sb, "(*");
// }

// static void transpile_cdecl(Datatype datatype, Identifier name_or_null) {

//     Datatype type_specifier = datatype;
//     while (true) {
//         if (type_specifier.kind == Typekind_Procedure) {
//             type_specifier = type_specifier.proc_ptr_typenode->procedure.return_type->solvedstate;
//         } else {
//             sbAppend(sb, getTypeCname(type_specifier));
//             u32 np = type_specifier.numPointers;
//             while (np-- > 0) sbAppend(sb, "*");
//             break;
//         }
//     }
// }

static void transpileFuncCall(ProcCall* call) {

    transpileExpression(call->proc_expr);
    if (call->proc && call->proc->overload) sbAppendSpan(sb, numberToString((u64)call->proc->overload));

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


static void transpilePrintFormat(Datatype type) {
    type = dealiasType(type);

    if (type.numPointers == 1) {
        if (type.kind == Typekind_char) { sbAppend(sb, "%s"); return; }
    }

    if (type.numPointers) {
        sbAppend(sb, "%p");
        return;
    }


    switch (type.kind) {
        case Typekind_AmbiguousDecimal: sbAppend(sb, "%f"); return;
        case Typekind_AmbiguousInteger: sbAppend(sb, "%d"); return;

        case Typekind_uint16: sbAppend(sb, "%hu"); return;
        case Typekind_uint32: sbAppend(sb, "%u"); return;
        case Typekind_uint64: sbAppend(sb, "%llu"); return;
        case Typekind_int16: sbAppend(sb, "%hd"); return;
        case Typekind_int32: sbAppend(sb, "%d"); return;
        case Typekind_int64: sbAppend(sb, "%lld"); return;

        case Typekind_float32: sbAppend(sb, "%f"); return;
        case Typekind_float64: sbAppend(sb, "%Lf"); return;
        case Typekind_char: sbAppend(sb, "%c"); return;

        default: break;
    }

    if (type.kind == Typekind_Struct) {
        Struct* stru = type.stru;
        sbAppend(sb, "{");
        foreach (field, stru->fields) {
            sbAppend(sb, get_string(field->name));
            sbAppend(sb, " = ");
            transpilePrintFormat(field->type->solvedstate);
            sbAppend(sb, ", ");
        }
        sb->length -= 2;
        sbAppend(sb, "}");
        return;
    }

    sbAppend(sb, "<print_error>");
}

static void transpilePrint(Expression** args) {
    sbAppend(sb, "printf(\"");
    {
        foreach (item, args) {
            Expression* arg = *item;
            transpilePrintFormat(arg->datatype);
        }
    }
    sbAppend(sb, "\"");


    foreach (item, args) {
        Expression* arg = *item;
        sbAppend(sb, ", ");
        if (arg->datatype.kind == Typekind_Struct && arg->datatype.numPointers == 0) {
            foreach (field, arg->datatype.stru->fields) {

                if (field->type->solvedstate.kind == Typekind_Struct && field->type->solvedstate.numPointers == 0) {
                    // TODO: print struct inside struct
                }

                transpileExpression(arg);
                sbAppend(sb, ".");
                sbAppend(sb, get_string(field->name));
                sbAppend(sb, ", ");
            }
            sb->length -= 2;
        } else {
            if (arg->expressionType == ExprType_Sizeof) sbAppend(sb, "(uint32)");
            transpileExpression(arg);
        }
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

            sbAppend(sb, get_string(var->name));
        } break;

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;

            if (deref->base.datatype.kind == Typekind_Enum) {
                EnumEntry* entry = getEnumEntry(deref->base.datatype._enum, deref->name);
                if (entry) { // TODO: this introduces a bug if there is an enumentry that coincidentaly has the same name as a struct field, solution: use deref->ref
                    sbAppendSpan(sb, numberToString(entry->value));
                    break;
                }
            }

            transpileExpression(deref->expr);

            if (deref->expr->datatype.numPointers) sbAppend(sb, "->");
            else sbAppend(sb, ".");



            // I belive this had to do with contextual inclusion of fields
            // if (deref->expr->datatype.kind == Typekind_Struct) {
            //     Struct* stru = deref->expr->datatype.stru;
            //     foreach (field, stru->fields) {
            //         if (field->name == deref->name) goto done;
                    
            //     }
            // }
            // done:

            sbAppend(sb, get_string(deref->name));
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;

            if (ind->indexed->datatype.kind == Typekind_Array) {
                sbAppend(sb, "((");
                Datatype elm_type = ind->indexed->datatype.array_typenode->array.element_type->solvedstate;
                elm_type.numPointers++;
                transpileDatatype(elm_type);
                sbAppend(sb, ")");
                transpileExpression(ind->indexed);
                sbAppend(sb, ".data)");
            } else {
                transpileExpression(ind->indexed);
            }

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
            sbAppend(sb, get_string(lit->data.string));
            sbAppend(sb, "\"");
        } break;

        case ExprType_Literal_Char: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendChar(sb, '\'');

            switch (lit->data.character) {
                case '\n': sbAppend(sb, "\\n"); break;
                case '\t': sbAppend(sb, "\\t"); break;
                case '\\': sbAppend(sb, "\\\\"); break;
                case '\'': sbAppend(sb, "\\'"); break;

                default:
                    sbAppendChar(sb, lit->data.character);
                    break;
            }

            sbAppendChar(sb, '\'');
        } break;

        case ExprType_Literal_True: sbAppend(sb, "1"); break;
        case ExprType_Literal_False: sbAppend(sb, "0"); break;

        case ExprType_Literal_Integer: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendSpan(sb, numberToString(lit->data.integer));
        } break;

        case ExprType_Literal_Decimal: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            char temp_buffer[24];
            u32 i = snprintf(temp_buffer, 23, "%f", lit->data.decimal);
            temp_buffer[i] = '\0';
            sbAppend(sb, temp_buffer);
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
                sbAppend(sb, "(");
                transpileExpression(p->innerExpr);
                sbAppend(sb, ")");
            }
        } break;

        case ExprType_Compound: {
            CompoundExpression* com = (CompoundExpression*)expr;

            if (com->base.datatype.kind != Typekind_Invalid) {
                sbAppend(sb, "(");
                transpileDatatype(com->base.datatype);
                sbAppend(sb, ") ");

                if (com->base.datatype.kind == Typekind_Array) {
                    sbAppend(sb, "{ .length = ");
                    u32 len = com->elements ? list_length(com->elements) : 0;
                    sbAppendSpan(sb, numberToString(len));
                    sbAppend(sb, ", .data = (");
                    transpileDatatype(com->base.datatype.array_typenode->array.element_type->solvedstate);
                    sbAppend(sb, "[])");
                }
            }

            sbAppend(sb, "{");
            if (com->elements) {
                foreach (elm, com->elements) {
                    if (elm->name) {
                        sbAppend(sb, ".");
                        sbAppend(sb, get_string(elm->name));
                        sbAppend(sb, " = ");
                    }
                    transpileExpression(elm->expr);
                    sbAppend(sb, ", ");
                }
                sb->length -= 2;
            } else {
                sbAppend(sb, "0");
            }
            sbAppend(sb, "}");

            if (com->base.datatype.kind == Typekind_Array) sbAppend(sb, "}");
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
    sbAppend(sb, "if ");
    transpileCondition(ifst->condition);
    sbAppend(sb, " ");

    if (ifst->then_statement) transpileStatement(ifst->then_statement);
    else sbAppend(sb, ";");

    if (ifst->else_statement) {
        sbAppend(sb, " else ");
        transpileStatement(ifst->else_statement);
    }
}

static void transpileStruct(Struct* stru) {
    sbAppend(sb, "struct ");
    sbAppend(sb, get_string(stru->name));
    sbAppend(sb, " {");

    tabing++;

    foreach (field, stru->fields) {
        newline();
        _transpileDatatype(field->type->solvedstate, field->name);
        sbAppend(sb, ";");
    }

    tabing--;
    newline();

    sbAppend(sb, "};");
}

static void transpileTypedef(Typedef* def) {
    sbAppend(sb, "typedef ");
    if (def->type) {
        _transpileDatatype(def->type->solvedstate, def->name);
    } else {
        sbAppend(sb, "struct ");
        char* name = get_string(def->name);
        sbAppend(sb, name);
        sbAppend(sb, " ");
        sbAppend(sb, name);
    }
    sbAppend(sb, ";");
}

static void transpileVarDecl(Declaration* decl) {

    if (decl->is_static) {
        list_add(static_decls, decl);
        sbAppend(sb, "// static decl");
        return;
    }

    _transpileDatatype(decl->type->solvedstate, decl->name);
    if (decl->expr) {
        sbAppend(sb, " = ");
        transpileExpression(decl->expr);
    }

    sbAppend(sb, ";");
}

static void transpileStatement(Statement* statement) {
    switch (statement->statementType) {
        case Statement_Declaration: {
            Declaration* decl = (Declaration*)statement;
            transpileVarDecl(decl);
        } break;

        case Statement_Struct: {
            Struct* stru = (Struct*)statement;
            sbAppend(sb, "typedef struct ");
            sbAppend(sb, get_string(stru->name));
            sbAppend(sb, " ");
            sbAppend(sb, get_string(stru->name));
            sbAppend(sb, ";");
            newline();
            transpileStruct(stru);
        } break;

        case Statement_Enum: {
            Enum* en = (Enum*)statement;
            sbAppend(sb, "typedef uint32 ");
            sbAppend(sb, get_string(en->name));
            sbAppend(sb, ";");
        } break;

        case Statement_Typedef: {
            transpileTypedef((Typedef*)statement);
        } break;

        case Statement_Constant: {
            sbAppend(sb, "/* local constant */");
        } break;

        case Statement_Assignment: {
            Assignment* ass = (Assignment*)statement;
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
            if (sta->statement) transpileStatement(sta->statement);
            else sbAppend(sb, ";");
        } break;
        case Statement_For: {
            ForStatement* forsta = (ForStatement*)statement;
            char* name = get_string(forsta->index_name);

            sbAppend(sb, "for (");

            if (forsta->index_type) transpileType(forsta->index_type);
            else transpileDatatype(type_int32); // TODO: change this

            sbAppend(sb, " ");
            sbAppend(sb, name);
            sbAppend(sb, " = ");
            transpileExpression(forsta->min_expr);
            sbAppend(sb, "; ");

            sbAppend(sb, name);
            sbAppend(sb, " < ");
            transpileExpression(forsta->max_expr);
            sbAppend(sb, "; ");

            sbAppend(sb, name);
            sbAppend(sb, "++) ");

            transpileStatement(forsta->statement);
        } break;

        case Statement_Switch: {
            SwitchStatement* switchSta = (SwitchStatement*)statement;
            sbAppend(sb, "switch ");
            transpileCondition(switchSta->expr);
            sbAppend(sb, " ");
            transpileScope(switchSta->scope);
        } break;

        case Statement_Procedure: {
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

        case Statement_Argument:
        case Statement_EnumEntry: {
            error(0, "internal(transpiler)", "Unreachable code. This is a bug!");
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

static void transpileFunctionSignature(Procedure* proc) {


    if (proc->name != builtin_string_main && proc->scope) sbAppend(sb, "static ");
    transpileType(proc->returnType);
    sbAppend(sb, " ");

    if (proc->name == builtin_string_main) sbAppend(sb, "__main");
    else sbAppend(sb, get_string(proc->name));

    if (proc->overload) sbAppendSpan(sb, numberToString((u64)proc->overload));

    sbAppend(sb, "(");
    if (proc->arguments) {
        foreach (arg, proc->arguments) {
            _transpileDatatype(arg->type->solvedstate, arg->name);
            sbAppend(sb, ", ");
        }
        sb->length -= 2;
    }
    sbAppend(sb, ")");
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
    sbAppend(sb, " ");
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

void transpile(Codebase* codebase) {
    // TODO: use a higer initial capacity for the string builder
    StringBuilder builder = sbCreate();
    sb = &builder;

    if (static_decls == null) static_decls = list_create(Declaration*);
    else list_clear(static_decls);

    // sbAppend(sb, "#include <stdlib.h>\n"); // malloc
    // sbAppend(sb, "#include <stdio.h>\n"); // printf
    // sbAppend(sb, "#define true 1\n#define false 0\n");

    sbAppend(sb, "\n// Basics\n");
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
    sbAppend(sb, "int printf(const char* format, ...);\n");
    sbAppend(sb, "typedef struct Array { void* data; uint32 length; } Array;\n");


    sbAppend(sb, "\n// Structs forward declarations\n");
    u32 structs_count = list_length(codebase->structs);
    for (u32 i = 0; i < structs_count; i++) {
        Struct* s = codebase->structs[i];
        char* name = get_string(s->name);
        sbAppend(sb, "typedef struct ");
        sbAppend(sb, name);
        sbAppend(sb, " ");
        sbAppend(sb, name);
        sbAppend(sb, ";\n");
    }

    sbAppend(sb, "\n// Enums\n");
    foreach (enu, codebase->enums) {
        Enum* en = *enu;
        sbAppend(sb, "typedef uint32 ");
        sbAppend(sb, get_string(en->name));
        sbAppend(sb, ";\n");
    }


    sbAppend(sb, "\n// Type aliases\n");
    foreach (type_def, codebase->type_defs) {
        transpileTypedef(*type_def);
        sbAppend(sb, "\n");
    }


    {
        sbAppend(sb, "\n// Structs\n");
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
                    sbAppend(sb, "\n");
                    transpiled++;
                }
            }
            dep++;
        }
    }

    sbAppend(sb, "\n// Forward declarations\n");
    u32 procs_count = list_length(codebase->procedures);
    for (u32 i = 0; i < procs_count; i++) {
        transpileFunctionSignature(codebase->procedures[i]);
        sbAppend(sb, ";\n");
    }

    sbAppend(sb, "\n// Declarations\n");
    foreach (global_var, codebase->global_vars) {
        Declaration* decl = *global_var;

        if (decl->expr && !isCompiletimeExpression(decl->expr)) {
            list_add(static_decls, decl);
            continue;
        }

        sbAppend(sb, "static ");
        _transpileDatatype(decl->type->solvedstate, decl->name);
        if (decl->expr) {
            sbAppend(sb, " = ");
            transpileExpression(decl->expr);
        }
        sbAppend(sb, ";\n");
    }

    {
        StringBuilder impl_sb = sbCreate(); // I am such a dirty trickster
        StringBuilder* temp = sb;
        sb = &impl_sb;

        sbAppend(sb, "\n// Implementations\n");
        for (u32 i = 0; i < procs_count; i++) {
            Procedure* proc = codebase->procedures[i];
            if (proc->scope == null) continue;
            transpileProcedure(proc);
        }

        sb = temp;

        foreach (static_decl, static_decls) {
            Declaration* decl = *static_decl;
            sbAppend(sb, "static ");
            _transpileDatatype(decl->type->solvedstate, decl->name);
            if (decl->expr && isCompiletimeExpression(decl->expr)) {
                sbAppend(sb, " = ");
                transpileExpression(decl->expr);
            }
            sbAppend(sb, ";\n");
        }

        sbAppend(sb, impl_sb.content);
        sbDestroy(&impl_sb);
    }

    sbAppend(sb, "static void __static_init() {");
    tabing++;
    foreach (static_decl, static_decls) {
        Declaration* decl = *static_decl;
        if (decl->expr && isCompiletimeExpression(decl->expr)) continue;
        newline();
        sbAppend(sb, get_string(decl->name));
        sbAppend(sb, " = ");
        transpileExpression(decl->expr);
        sbAppend(sb, ";");
    }
    tabing--;
    newline();
    sbAppend(sb, "}\n");

    char* c_main_code =
        "int main(int argc, char** argv) {\n"
        "    __static_init();\n"
        "    __main();\n"
        "    return 0;\n"
        "}";

    sbAppend(sb, c_main_code);


    FILE* file;
    if ( !fopen_s(&file, "output.g.c", "w") ) {
        fprintf(file, "%s", sb->content);
        fclose(file);
    } else {
        printf("Could not write to output.g.c\n");
    }

    sbDestroy(sb);
}