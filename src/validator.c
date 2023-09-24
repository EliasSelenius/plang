


static Datatype validateExpression(Expression* expr);
static Datatype validateExpressionEx(Expression* expr, Datatype expected_type);
static bool typeAssignable(Datatype toType, Datatype fromType);
static void validateScope(Scope* scope);
static void validateProcedure(Procedure* proc);


static bool typeAssignable(Datatype toType, Datatype fromType) {
    toType = dealiasType(toType);
    fromType = dealiasType(fromType);

    if (fromType.kind == Typekind_Invalid) return true;
    if (toType.kind == Typekind_Invalid) return true;

    if (typeEquals(toType, fromType)) return true;

    if (toType.numPointers != fromType.numPointers) return false;
    u32 numPointers = toType.numPointers;

    if (numPointers) { // if we are a pointer of any degree

        if (toType.kind == Typekind_void || fromType.kind == Typekind_void) return true;

        if (toType.kind == Typekind_Procedure && fromType.kind == Typekind_Procedure) {
            // if (procSignatureAssignable(toType.procedure, fromType.procedure)) return true;
            return true; // TODO: ...
        }

        return false;
    } // if we are a value-type:


    // Ambiguous cases
    if (fromType.kind == Typekind_AmbiguousInteger) switch (toType.kind) {
        case Typekind_uint8:
        case Typekind_uint16:
        case Typekind_uint32:
        case Typekind_uint64:
        case Typekind_int8:
        case Typekind_int16:
        case Typekind_int32:
        case Typekind_int64:
        case Typekind_AmbiguousDecimal:
        case Typekind_float32:
        case Typekind_float64: return true;
        default: return false;
    } else if (fromType.kind == Typekind_AmbiguousDecimal) switch (toType.kind) {
        case Typekind_float32:
        case Typekind_float64: return true;
        default: return false;
    }

    if (isIntegralType(fromType)) {

        if (toType.kind == Typekind_float32 ||
            toType.kind == Typekind_float64 ||
            toType.kind == Typekind_AmbiguousDecimal) return true;

        if (isIntegralType(toType))
            if (rangein(getNumericDomain(fromType), getNumericDomain(toType)))
                return true;

        return false;
    }

    if (fromType.kind == Typekind_float32 && toType.kind == Typekind_float64) return true;

    return false;
}


static char* get_basic_type_string(Datatype type) {
    switch (type.kind) {
        case Typekind_Invalid:          return "Invalid-Type";
        case Typekind_AmbiguousInteger: return "Ambiguous-Integer";
        case Typekind_AmbiguousDecimal: return "Ambiguous-Decimal";

        case Typekind_uint8:            return "uint8";
        case Typekind_uint16:           return "uint16";
        case Typekind_uint32:           return "uint32";
        case Typekind_uint64:           return "uint64";
        case Typekind_int8:             return "int8";
        case Typekind_int16:            return "int16";
        case Typekind_int32:            return "int32";
        case Typekind_int64:            return "int64";
        case Typekind_float32:          return "float32";
        case Typekind_float64:          return "float64";
        case Typekind_void:             return "void";
        case Typekind_char:             return "char";

        case Typekind_Struct:           return get_string(type.stru->name);
        case Typekind_Enum:             return get_string(type._enum->name);
        case Typekind_Typedef:          return get_string(type.type_def->name);

        default: return null;
    }
}

static void construct_type_string(Datatype type, StringBuilder* sb) {

    Type* typenode = type.array_typenode; // same as proc_ptr_typenode

    switch (type.kind) {
        case Typekind_Procedure: {
            construct_type_string(typenode->procedure.return_type->solvedstate, sb);
            sbAppend(sb, "(");
            Type* arg = typenode->procedure.first_argument;
            if (arg) {
                do {
                    construct_type_string(arg->solvedstate, sb);
                    sbAppend(sb, ", ");
                    arg = arg->next;
                } while (arg);
                sb->length -= 2; // trim of trailing comma: ", "
            }
            sbAppend(sb, ")");

            type.numPointers--; // proc pointers have an implicit asterix
        } break;


        case Typekind_Array:         construct_type_string(typenode->array.element_type->solvedstate, sb); sbAppend(sb, "[]"); break;
        case Typekind_Fixed_Array:   construct_type_string(typenode->array.element_type->solvedstate, sb);
                                     sbAppend(sb, "[0]");//  sbAppend(sb, numberToString()) // TODO: append array size
                                     break;
        case Typekind_Dynamic_Array: construct_type_string(typenode->array.element_type->solvedstate, sb); sbAppend(sb, "[..]"); break;

        default:
            char* typename = get_basic_type_string(type);
            sbAppend(sb, typename);
            break;
    }

    u32 np = type.numPointers;
    while (np-- != 0) sbAppendChar(sb, '*');
}

static StringBuilder* temp_builder() {

    static u32 calls = 0;
    static const u32 builders_count = 2;
    static StringBuilder builders[builders_count] = {0};

    StringBuilder* sb = &builders[calls++ % builders_count];

    if (sb->content == null) {
        *sb = sbCreate();
        printf("[DEBUG]: Initialized temporary string builder. This should only happen %u times.\n", builders_count);
    }

    sbClear(sb);
    return sb;
}

static void assertAssignability(Datatype toType, Datatype fromType, void* node) {
    if (!typeAssignable(toType, fromType)) {

        StringBuilder *to_sb, *from_sb;
        to_sb = temp_builder();
        from_sb = temp_builder();

        construct_type_string(toType, to_sb);
        construct_type_string(fromType, from_sb);

        if (node) {
            error_node(node, "Type missmatch. \"%s\" is not assignable to \"%s\".", from_sb->content, to_sb->content);
        } else {
            error_temp("Type missmatch. \"%s\" is not assignable to \"%s\".", from_sb->content, to_sb->content);
        }
    }
}

static Datatype typeofStatement(Statement* sta) {

    if (!sta) return type_invalid;

    switch (sta->statementType) {
        case Statement_FixedArray:
        case Statement_Declaration: {
            Declaration* decl = (Declaration*)sta;
            return decl->type->solvedstate;
        }

        case Statement_Constant: {
            Declaration* decl = (Declaration*)sta;
            return decl->expr->datatype;
        }

        case Statement_Argument: {
            ProcArg* arg = (ProcArg*)sta;
            return arg->type->solvedstate;
        }

        case Statement_Procedure: {
            Procedure* proc = (Procedure*)sta;
            return proc->type_node->solvedstate;
        }

        case Statement_EnumEntry: {
            EnumEntry* entry = (EnumEntry*)sta;
            return (Datatype) { .kind = Typekind_Enum, ._enum = entry->_enum };
        }

        case Statement_For: {
            ForStatement* forsta = (ForStatement*)sta;
            return forsta->index_type ? forsta->index_type->solvedstate : type_int32; // NOTE: hardcoded default type here.
        }

        default: break;
    }

    return type_invalid;
}


static Procedure* searchMatchingOverload(Procedure* proc, ProcCall* call) {
    u32 argslen = call->args ? list_length(call->args) : 0;

    do {
        if (argslen != (proc->arguments ? list_length(proc->arguments) : 0)) continue;

        for (u32 i = 0; i < argslen; i++)
            if (!typeAssignable(proc->arguments[i].type->solvedstate, call->args[i]->datatype)) goto next;

        return proc;
        next:;
    } while ((proc = proc->next_overload));

    return null;
}


/*

foo()
foo.bar()

void bar(with Foo foo) {

}

Foo foo;
foo.bar();

*/

static Procedure* get_possibly_overloaded_proc(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            if (var->ref->statementType == Statement_Procedure) return (Procedure*)var->ref;
            return null;
        }


        // TODO: when contextual-inclusion on arguments are implemented, then this expression may return a Procedure that could be overloaded
        // case ExprType_Deref: {
        //     DerefOperator* deref = (DerefOperator*)expr;
        //     if (deref->expr) 
        //     return null;
        // }

        default: return null;
    }
}

static Datatype validateProcCall(ProcCall* call) {

    // validate passed arguments
    u32 argslength = call->args ? list_length(call->args) : 0;
    for (u32 i = 0; i < argslength; i++) validateExpression(call->args[i]);

    Datatype type = validateExpression(call->proc_expr);


    // builtin procedures
    if (call->proc_expr->expressionType == ExprType_Variable) {
        VariableExpression* var = (VariableExpression*)call->proc_expr;
        if (var->name == builtin_string_print) return type_void;
    }


    // Special case for overloaded procedures
    Procedure* proc = get_possibly_overloaded_proc(call->proc_expr);
    if (proc) {
        Procedure* correct_overload = searchMatchingOverload(proc, call);
        if (correct_overload) {
            call->proc = correct_overload;
            return correct_overload->returnType->solvedstate;
        }

        error_node(call, "No valid overload found for \"%s\"", get_string(proc->name));
        return type_invalid;
    }


    type = dealiasType(type); // TODO: what if it is a double pointer? Then this shouldnt work.
    if (type.kind == Typekind_Invalid) return type;

    if (type.kind != Typekind_Procedure) {
        error_node(call, "Invalid procedurecall. This expression is not a procedure.");
        return type_invalid;
    }

    // TODO: typecheck arguments
    // return type.procedure->return_type;
    return type.proc_ptr_typenode->procedure.return_type->solvedstate;
}

static Datatype _validateExpression(Expression* expr, Datatype expected_type) {

    switch (expr->expressionType) {

        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            var->base.datatype = typeofStatement(var->ref);
            return var->base.datatype;
        }

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;

            // implicit derefing:
            if (deref->expr == null) {
                if (expected_type.kind == Typekind_Enum) {
                    EnumEntry* entry = getEnumEntry(expected_type._enum, deref->name);
                    return (Datatype) { Typekind_Enum, ._enum = entry->_enum };
                }

                error_node(deref, "Cannot infer implicit dereferencing \".%s\".", get_string(deref->name));
                return type_invalid;
            }

            Datatype inner_expr_type = validateExpression(deref->expr);

            // when we deref a variable expression
            if (deref->expr->expressionType == ExprType_Variable) {
                VariableExpression* var = (VariableExpression*)deref->expr;
                if (var->ref->statementType == Statement_Enum) {
                    EnumEntry* entry = getEnumEntry((Enum*)var->ref, deref->name);
                    return (Datatype) { Typekind_Enum, ._enum = entry->_enum };
                }
            }

            if (inner_expr_type.kind == Typekind_Struct) {

                if (inner_expr_type.numPointers > 1) {
                    error_node(expr, "Attempted to dereference a %dth-degree pointer.", inner_expr_type.numPointers);
                    return type_invalid;
                }

                Declaration* field = (Declaration*)getMember(inner_expr_type.stru, deref->name);
                if (field) return field->type->solvedstate;

                error_node(expr, "Field \"%s\" does not exist on type \"%s\".", get_string(deref->name), get_string(inner_expr_type.stru->name));
                return type_invalid;
            }

            error_node(expr, "Invalid dereferencing.");
            return type_invalid;
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;

            Datatype indexedType = validateExpression(ind->indexed);
            Datatype indexType = validateExpression(ind->index);
            // TODO: is indexType a valid integer expression


            if (indexedType.numPointers == 0) {

                if ((indexedType.kind == Typekind_Array) ||
                    (indexedType.kind == Typekind_Fixed_Array) ||
                    (indexedType.kind == Typekind_Dynamic_Array)) {

                    return indexedType.array_typenode->array.element_type->solvedstate;
                }

                error_node(expr, "Attempted to index an expression that is not indexable.");
                return type_invalid;
            }


            Datatype resType = indexedType;
            resType.numPointers--;
            return resType;
        } break;

        case ExprType_Unary_Negate: {
            UnaryExpression* unary = (UnaryExpression*)expr;
            Datatype type = validateExpression(unary->expr);
            // TODO: can type be negated?
            return type;
        } break;

        case ExprType_Unary_BitwiseNot: {
            UnaryExpression* unary = (UnaryExpression*)expr;
            Datatype type = validateExpression(unary->expr);
            // TODO: can everything be bit flipped like this?
            return type;
        } break;

        case ExprType_Unary_PreIncrement:
        case ExprType_Unary_PostIncrement:
        case ExprType_Unary_PreDecrement:
        case ExprType_Unary_PostDecrement: {
            UnaryExpression* unary = (UnaryExpression*)expr;
            Datatype type = validateExpression(unary->expr);
            // TODO: can type be incremented/decremented?
            return type;
        } break;

        case ExprType_Unary_Not: {
            UnaryExpression* unary = (UnaryExpression*)expr;
            Datatype type = validateExpression(unary->expr);
            // TODO: is type a boolean type?
            return type; // TODO: return boolean
        } break;
        case ExprType_Unary_AddressOf: {
            UnaryExpression* unary = (UnaryExpression*)expr;
            Datatype type = validateExpression(unary->expr);
            // TODO: is unary->expr an expression we can take the address of?

            type.numPointers++;
            return type;
        } break;
        case ExprType_Unary_ValueOf: {
            UnaryExpression* unary = (UnaryExpression*)expr;
            Datatype type = validateExpression(unary->expr);
            // TODO: is unary->expr an expression we can take the value of?

            type.numPointers--;
            return type;
        } break;

        case ExprType_Less:
        case ExprType_Greater:
        case ExprType_LessEquals:
        case ExprType_GreaterEquals:
        case ExprType_Equals:
        case ExprType_NotEquals:
        case ExprType_BooleanAnd:
        case ExprType_BooleanOr: {
            BinaryExpression* bop = (BinaryExpression*)expr;
            Datatype leftType = validateExpressionEx(bop->left, expected_type);
            Datatype rightType = validateExpressionEx(bop->right, expected_type);

            // TODO: valid operands?

            (void)leftType;
            (void)rightType;

            return type_bool;
        } break;

        case ExprType_Bitwise_And:
        case ExprType_Bitwise_Or:
        case ExprType_Bitwise_Xor:
        case ExprType_Bitwise_Lshift:
        case ExprType_Bitwise_Rshift:
        case ExprType_Plus:
        case ExprType_Minus:
        case ExprType_Mul:
        case ExprType_Div:
        case ExprType_Mod: {
            BinaryExpression* bop = (BinaryExpression*)expr;
            Datatype leftType = validateExpressionEx(bop->left, expected_type);
            Datatype rightType = validateExpressionEx(bop->right, expected_type);

            // TODO: is a valid operator operands pair?

            if (typeEquals(leftType, rightType)) return leftType;


            Datatype numberType = mergeNumberTypes(leftType, rightType);
            if (numberType.kind != Typekind_Invalid) return numberType;


            bool isLeftAssignable = typeAssignable(leftType, rightType);
            if (isLeftAssignable) return leftType;
            else {
                bool isRightAssignable = typeAssignable(rightType, leftType);
                if (isRightAssignable) return rightType;
            }

            error_node(expr, "Invalid binary expression.");
            return type_invalid;
        } break;

        case ExprType_Alloc: {
            AllocExpression* alloc = (AllocExpression*)expr;

            // recurse on possibe size-subexpression
            if (alloc->sizeExpr) {
                Datatype size = validateExpression(alloc->sizeExpr);
                // TODO: is this a valid integer expression?
            }

            Datatype type = alloc->type->solvedstate;
            type.numPointers++;
            return type;
        } break;

        case ExprType_Sizeof: {
            SizeofExpression* sof = (SizeofExpression*)expr;
            return type_uint64;
        } break;

        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            Datatype conditionType = validateExpression(ter->condition);
            Datatype thenType = validateExpression(ter->thenExpr);
            Datatype elseType = validateExpression(ter->elseExpr);

            // TODO: is condition a valid boolean expression
            // TODO: does thenExpr and elseExpr have a common type? 

            return thenType;
        } break;

        case ExprType_ProcCall: {
            ProcCall* fc = (ProcCall*)expr;
            return validateProcCall(fc);
        } break;

        case ExprType_Cast: {
            CastExpression* cast = (CastExpression*)expr;
            // TODO: is this a valid casting operation

            Datatype type = validateExpression(cast->expr);

            return cast->castToType->solvedstate;
        } break;

        { // literals
            case ExprType_Literal_Integer: return type_ambiguousInteger;
            case ExprType_Literal_Decimal: return type_ambiguousDecimal;
            case ExprType_Literal_Char:    return type_char;
            case ExprType_Literal_String:  return type_charPointer;
            case ExprType_Literal_True:    return type_bool;
            case ExprType_Literal_False:   return type_bool;
            case ExprType_Literal_Null:    return type_voidPointer;
        }

        case ExprType_Parenthesized: {
            ParenthesizedExpression* p = (ParenthesizedExpression*)expr;
            return validateExpression(p->innerExpr);
        } break;

        case ExprType_Compound: {
            CompoundExpression* com = (CompoundExpression*)expr;

            if (com->elements) {
                foreach (el, com->elements) {
                    validateExpression(el->expr);
                }
            }

            // TODO: validate if this compound is compatible with expected_type
            return expected_type;
        } break;
    }

    // this should never happen
    error_node(expr, "Unknown expression type. This is a bug!");
    return type_invalid;
}

static Datatype validateExpression(Expression* expr) {
    expr->datatype = _validateExpression(expr, type_invalid);
    return expr->datatype;
}

static Datatype validateExpressionEx(Expression* expr, Datatype expected_type) {
    expr->datatype = _validateExpression(expr, expected_type);
    return expr->datatype;
}

// static void assignment(Datatype toType, Expression* expr) {

// }

static void validateDeclaration(Declaration* decl) {

    if (decl->base.statementType == Statement_FixedArray) {
        decl->type->solvedstate.numPointers++; // because this is a fixed array declaration.
        // NOTE: decl->expr is used as the size expression for this fixed array
        validateExpression(decl->expr);
        return;
    }

    if (decl->expr) {

        Datatype type = validateExpressionEx(decl->expr, decl->type->solvedstate);
        if (type.kind == Typekind_Invalid) return;

        if (decl->type->node_type == TypeNode_MustInfer) {
            decl->type->solvedstate = resolveTypeAmbiguity(type);
        } else {
            assertAssignability(decl->type->solvedstate, type, decl->expr);
        }
    }
}

static void validateStruct(Struct* stru) {
    u32 fieldLen = list_length(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Declaration* field = &stru->fields[i];

        // TODO: field may be an alias
        if (field->type->solvedstate.kind == Typekind_Struct && field->type->solvedstate.numPointers == 0) {
            if (stru == field->type->solvedstate.stru) {
                error_node(field, "Struct \"%s\" self reference by value.", get_string(stru->name));
            }
        }
    }
}

static void validateEnum(Enum* en) {
    u64 entry_value = 0;

    foreach (entry, en->entries) {
        if (entry->expr) {
            validateExpression(entry->expr); // TODO: evaluate expression value
        }
        entry->value = entry_value++;
    }
}

static void validateStatement(Statement* sta) {
    switch (sta->statementType) {
        case Statement_FixedArray: {
            Declaration* decl = (Declaration*)sta;

            decl->type->solvedstate.numPointers++; // because this is a fixed array declaration.

            // NOTE: decl->expr is used as the size expression for this fixed array
            Datatype sizetype = validateExpression(decl->expr);
            // TODO: is asstype a valid integer expression?
        } break;

        case Statement_Declaration: {
            Declaration* decl = (Declaration*)sta;
            validateDeclaration(decl);
        } break;

        case Statement_Assignment: {
            Assignment* ass = (Assignment*)sta;

            Datatype toType = validateExpression(ass->assigneeExpr);
            Datatype fromType = validateExpressionEx(ass->expr, toType);

            assertAssignability(toType, fromType, ass->expr);
        } break;

        case Statement_Struct: {
            validateStruct((Struct*)sta);
        } break;

        case Statement_Enum: {
            validateEnum((Enum*)sta);
        } break;

        case Statement_Typedef: {
            Typedef* def = (Typedef*)sta;
        } break;

        case Statement_Constant: {
            Declaration* decl = (Declaration*)sta;
            validateExpression(decl->expr);
        } break;


        case Statement_Scope: {
            Scope* scope = (Scope*)sta;
            validateScope(scope);
        } break;

        case Statement_If: {
            IfStatement* ifsta = (IfStatement*)sta;

            // TODO: check if condition is a boolean expression
            validateExpression(ifsta->condition);
            if (ifsta->then_statement) validateStatement(ifsta->then_statement);
            if (ifsta->else_statement) validateStatement(ifsta->else_statement);

        } break;
        case Statement_While: {
            WhileStatement* whileSta = (WhileStatement*)sta;
            // TODO: check if condition is a boolean expression
            validateExpression(whileSta->condition);
            if (whileSta->statement) validateStatement(whileSta->statement);
        } break;
        case Statement_For: {
            ForStatement* forsta = (ForStatement*)sta;

            validateExpression(forsta->min_expr);
            validateExpression(forsta->max_expr);

            Datatype type = forsta->index_type
                          ? forsta->index_type->solvedstate
                          : type_int32;

            validateStatement(forsta->statement);
        } break;

        case Statement_Switch: {
            SwitchStatement* switchSta = (SwitchStatement*)sta;
            // TODO: check if expr is a valid type to switch on
            validateExpression(switchSta->expr);
            validateScope(switchSta->scope);
        } break;

        case Statement_Procedure: {
            Procedure* proc = (Procedure*)sta;

            init_typenode_for_proc(proc);
            validateProcedure(proc);
        } break;

        case Statement_Break: {
            // TODO: is inside loop?
        } break;
        case Statement_Continue: {
            // TODO: is inside loop?
        } break;
        case Statement_Return: {
            ReturnStatement* retSta = (ReturnStatement*)sta;

            Datatype type = type_void;
            if (retSta->returnExpr) type = validateExpressionEx(retSta->returnExpr, parser.procedure->returnType->solvedstate);

            if (type.kind == Typekind_Invalid) break;

            if (parser.procedure->returnType->node_type == TypeNode_MustInfer) {
                parser.procedure->returnType->solvedstate = resolveTypeAmbiguity(type);
            } else {
                if (!typeAssignable(parser.procedure->returnType->solvedstate, type)) {
                    error_node(sta, "Return type missmatch in function \"%s\".", get_string(parser.procedure->name));
                }
            }
        } break;

        case Statement_Goto: {
            // TODO: does label exist?
        } break;
        case Statement_Label: {
            // TODO: is label already declared?
        } break;
        case Statement_CaseLabel: {
            CaseLabelStatement* caseLabel = (CaseLabelStatement*)sta;

            if (caseLabel->switch_statement == null) {
                error_node(caseLabel, "Case label not declared inside switch.");
                break;
            }

            validateExpressionEx(caseLabel->expr, caseLabel->switch_statement->expr->datatype);

            // TODO: is compiletime expr?
        } break;
        case Statement_DefaultLabel: {
            // TODO: is inside switch?
        } break;

        case Statement_Expression: {
            StatementExpression* staExpr = (StatementExpression*)sta;
            validateExpression(staExpr->expr);
        } break;
    }
}

static void validateScope(Scope* scope) {
    for (u32 i = 0; i < list_length(scope->statements); i++) {
        Statement* sta = scope->statements[i];
        validateStatement(sta);
    }
}

static void validateProcedure(Procedure* proc) {
    if (proc->scope) {
        Procedure* otherproc = parser.procedure;
        parser.procedure = proc;

        validateScope(proc->scope);

        parser.procedure = otherproc;
    }

    if (proc->returnType->node_type == TypeNode_MustInfer) {
        proc->returnType->solvedstate = type_void;
    }
}


static void validate(Codebase* cb) {

    // validate structs
    u32 struLen = list_length(cb->structs);
    for (u32 i = 0; i < struLen; i++) {
        validateStruct(cb->structs[i]);
    }

    foreach (en, cb->enums) {
        validateEnum(*en);
    }

    foreach (decl, cb->global_vars) {
        validateDeclaration(*decl);
    }

    foreach (con, cb->global_consts) {
        validateExpression((*con)->expr);
    }

    parser.procedure = null;
    foreach (proc, cb->procedures) {
        validateProcedure(*proc);
    }
}