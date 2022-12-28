
typedef enum Vartype {
    Vartype_Variable,
    Vartype_ProcArg,
    Vartype_Constant,
    Vartype_Procedure
} Vartype;

typedef struct Variable {
    Vartype vartype;
    Identifier name;
    Datatype type;
} Variable;

static Procedure* procedure;
static Variable* variables;

static u32 capture_index;
static LocalProc* current_localproc = null;

static void registerVariable(Identifier name, Datatype type, Vartype vartype) {
    Variable var;
    var.vartype = vartype;
    var.name = name;
    var.type = type;
    list_add(variables, var);
}

static Datatype getVariable(Identifier name) {

    // look for local var
    if (variables) {
        u32 len = list_length(variables);
        for (u32 i = 0; i < len; i++) {
            if (name == variables[i].name) {

                if (current_localproc) {
                    if (i < capture_index) {
                        CapturedVariable cvar;
                        cvar.name = name;
                        cvar.type = variables[i].type;
                        list_add(current_localproc->captures, cvar);
                    }
                }

                return variables[i].type;
            }
        }
    }

    return type_invalid;
}

static Datatype validateExpression(Expression* expr);
static bool typeAssignable(Datatype toType, Datatype fromType);
static void validateScope(Scope* scope);
static void validateProcedure(Procedure* proc);
static void validateLocalProc(LocalProc* localproc);

static Datatype dealiasType(Datatype type) {
    if (type.kind == Typekind_Alias) {
        Datatype newType = current_namespace->aliases[type.ref].aliasedType;
        newType.numPointers += type.numPointers;
        return dealiasType(newType);
    }
    return type;
}

static void validateType(Datatype* type) {
    switch (type->kind) {

        case Typekind_Undecided: {

            foreach (stru, current_namespace->structs) {
                if (stru->name == type->ref) {
                    type->kind = Typekind_Struct;
                    type->ref = stru_index;
                    return;
                }
            }

            // TODO: enums

            foreach (alias, current_namespace->aliases) {
                if (alias->name == type->ref) {
                    type->kind = Typekind_Alias;
                    type->ref = alias_index;
                    return;
                }
            }

            foreach (optype, current_namespace->opaque_types) {
                if (*optype == type->ref) {
                    type->kind = Typekind_Opaque;
                    if (type->numPointers == 0) error_temp("Invalid usage of opaque type. You cannot use opaque types by-value.");
                    return;
                }
            }
        } break;

        // case Typekind_FuncPtr: {
        //     FuncPtr* f = getFuncPtr(type->ref);
        //     validateType(&f->returnType);
        //     for (u32 i = 0; i < f->argCount; i++) validateType(&f->argTypes[i]);
        // } return;

        default: return;
    }

    type->kind = Typekind_Invalid;
    // TODO: print the name here
    error_temp("Type not recognized.");
}

static bool funcPtrAssignable(ProcPtr* to, ProcPtr* from) {
    if (to->argCount != from->argCount) return false;

    if (!typeAssignable(to->returnType, from->returnType)) return false;

    for (u32 i = 0; i < to->argCount; i++) {
        if (!typeAssignable(to->argTypes[i], from->argTypes[i])) return false;
    }

    return true;
}

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

        if (toType.kind == Typekind_ProcPtr && fromType.kind == Typekind_ProcPtr) {
            ProcPtr* toPtr = getProcPtr(toType.ref);
            ProcPtr* fromPtr = getProcPtr(fromType.ref);
            if (funcPtrAssignable(toPtr, fromPtr)) return true;
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

static void constructTypename(char* buffer, u32 size, Datatype type) {
    char* typename = null;
    switch (type.kind) {
        case Typekind_Invalid: typename = "error_invalid"; break;
        case Typekind_Undecided: typename = "error_undecided"; break;
        case Typekind_MustBeInfered: typename = "error_mustinfer"; break;
        case Typekind_AmbiguousInteger: typename = "error_ambint"; break;
        case Typekind_AmbiguousDecimal: typename = "error_ambdec"; break;

        case Typekind_uint8: typename = "uint8"; break;
        case Typekind_uint16: typename = "uint16"; break;
        case Typekind_uint32: typename = "uint32"; break;
        case Typekind_uint64: typename = "uint64"; break;
        case Typekind_int8: typename = "int8"; break;
        case Typekind_int16: typename = "int16"; break;
        case Typekind_int32: typename = "int32"; break;
        case Typekind_int64: typename = "int64"; break;
        case Typekind_float32: typename = "float32"; break;
        case Typekind_float64: typename = "float64"; break;
        case Typekind_void: typename = "void"; break;
        case Typekind_char: typename = "char"; break;

        case Typekind_Struct: typename = get_string(current_namespace->structs[type.ref].name); break;
        case Typekind_Enum: typename = null; break;
        case Typekind_Alias: typename = get_string(current_namespace->aliases[type.ref].name); break;
        case Typekind_Opaque: typename = get_string(type.ref); break;
        case Typekind_ProcPtr: typename = "SomeFuncPtr"; break;

    }

    snprintf(buffer, size, "%s%.*s", typename, type.numPointers, "**********");
}

static void assertAssignability(Datatype toType, Datatype fromType, void* node) {
    if (!typeAssignable(toType, fromType)) {

        char toTypeName[50];
        char fromTypeName[50];
        constructTypename(toTypeName, sizeof(toTypeName), toType);
        constructTypename(fromTypeName, sizeof(fromTypeName), fromType);

        if (node) {
            error_node(node, "Type missmatch. \"%s\" is not assignable to \"%s\".", fromTypeName, toTypeName);
        } else {
            error_temp("Type missmatch. \"%s\" is not assignable to \"%s\".", fromTypeName, toTypeName);
        }
    }
}

static Procedure* getProcOverload(Namespace* ns, Identifier name, ProcCall* call) {

    u32 argslen = call->args ? list_length(call->args) : 0;
    foreach (proc, ns->procedures) {
        if (proc->name == name) {
            u32 len = proc->arguments ? list_length(proc->arguments) : 0;
            if (argslen != len) continue;
            if (len) {
                foreach (arg, proc->arguments) {
                    if (!typeAssignable(arg->type, call->args[arg_index]->datatype)) goto next;
                }
            }
            return proc;
        }
        next: continue;
    }

    return null;
}

static Datatype validateProcCall(ProcCall* call) {
    call->overload = 0;

    // validate passed arguments
    u32 argslength = call->args ? list_length(call->args) : 0;
    for (u32 i = 0; i < argslength; i++) validateExpression(call->args[i]);

    // Special case for overloaded procedures or builtin procedures
    if (call->proc_expr->expressionType == ExprType_Variable) {
        VariableExpression* var = (VariableExpression*)call->proc_expr;
        Namespace* ns = getNamespace(var->namespace_name);

        // If it's the default namespace
        if (ns == g_Codebase.namespaces[0]) {
            // builtin procedures:
            if (var->name == builtin_print_name) return type_void;
        }

        // overloaded procedures:
        Procedure* proc = getProcOverload(ns, var->name, call);
        if (proc) {
            call->overload = proc->overload;
            return proc->returnType;
        }

        // if no procedure is found we continue as usual
    }

    Datatype type = validateExpression(call->proc_expr);
    type = dealiasType(type);
    // TODO: what if it is a double pointer? Then this shouldnt work.

    if (type.kind == Typekind_Invalid) return type;
    if (type.kind != Typekind_ProcPtr) {
        error_node(call, "Invalid procedurecall. This expression is not a procedure.");
    }

    ProcPtr* proc = getProcPtr(type.ref);
    // TODO: typecheck arguments
    return proc->returnType;
}

// static Datatype validateFuncCall(ProcCall* call) {
//     call->overload = 0;

//     // validate passed arguments
//     u32 passedArgumentsLength = call->args ? list_length(call->args) : 0;
//     Datatype passedArguments[passedArgumentsLength]; // TODO: we store the datatype in Expression now, so we can change this code.
//     for (u32 i = 0; i < passedArgumentsLength; i++) passedArguments[i] = validateExpression(call->args[i]);


//     if (call->proc_expr->expressionType != ExprType_Variable) goto funcptrPart;
//     Identifier name = ((VariableExpression*)call->proc_expr)->name;

//     if (name == builtin_print_name) return type_void;

//     // look for function with possible overloads
//     u32 len = list_length(current_namespace->procedures);
//     for (u32 i = 0; i < len; i++) {
//         Procedure* proc = &current_namespace->procedures[i];
//         if (name != proc->name) continue;

//         u32 expectedArgumentLength = proc->arguments ? list_length(proc->arguments) : 0;

//         if (proc->overload) {
//             if (passedArgumentsLength != expectedArgumentLength) continue;

//             bool compat = true;
//             for (u32 i = 0; i < passedArgumentsLength; i++) {
//                 if (!typeAssignable(proc->arguments[i].type, passedArguments[i])) {compat = false; break;}
//             }

//             if (!compat) continue;
//             call->overload = proc->overload;
//         } else {
//             if (passedArgumentsLength != expectedArgumentLength) {
//                 error_temp("Unexpected number of arguments passed to \"%s\", expected %d, but got %d.",
//                     get_string(name),
//                     expectedArgumentLength, passedArgumentsLength);
//             } else {
//                 for (u32 i = 0; i < passedArgumentsLength; i++) assertAssignability(proc->arguments[i].type, passedArguments[i], call->args[i]);
//             }
//         }

//         return proc->returnType;
//     }

//     funcptrPart:
//     Datatype calleeType = dealiasType(validateExpression(call->proc_expr));
//     if (calleeType.kind == Typekind_ProcPtr) { // what if it is a double pointer? Then this shouldnt work.
//         call->base.expressionType = ExprType_ProcPtrCall;
//         ProcPtr* funcptr = getProcPtr(calleeType.ref);

//         if (passedArgumentsLength != funcptr->argCount) {
//             error_temp("Unexpected number of arguments passed to function pointer, expected %d, but got %d.",
//                 funcptr->argCount, passedArgumentsLength);
//             return funcptr->returnType;
//         }

//         for (u32 i = 0; i < passedArgumentsLength; i++) assertAssignability(funcptr->argTypes[i], passedArguments[i], call->args[i]);
//         return funcptr->returnType;
//     }


//     error_temp("Function \"%s\" was not found.", get_string(name));
//     return type_invalid;
// }

static Datatype _validateExpression(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;

            if (var->namespace_name == 0) {
                Datatype type = getVariable(var->name);
                if (type.kind != Typekind_Invalid) return type;
            }

            Namespace* ns = getNamespace(var->namespace_name);
            Datatype type = getThing(ns, var->name); // TODO: how should the compiler determine which overload the user refers to?

            if (type.kind == Typekind_Invalid)
                error_node(var, "\"%s\" is not declared.", get_string(var->name));

            return type;
        } break;

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            Datatype datatype = validateExpression(deref->expr);
            if (datatype.kind == Typekind_Invalid) return type_invalid;

            if (datatype.numPointers > 1) {
                error_node(expr, "Attempted to dereference a %dth-degree pointer.", datatype.numPointers);
                return type_invalid;
            }

            if (datatype.kind != Typekind_Struct) {
                error_node(expr, "Invalid dereferencing.");
                return type_invalid;
            }

            PlangStruct* stru = &current_namespace->structs[datatype.ref];
            Field* field = getField(stru, deref->name);
            if (!field) {
                error_node(expr, "Field \"%s\" does not exist on type \"%s\".",
                        get_string(deref->name),
                        get_string(stru->name));
                return type_invalid;
            }
            return field->type;
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;
            Datatype indexedType = validateExpression(ind->indexed);
            if (indexedType.numPointers == 0) {
                error_node(expr, "Attempted to index something that isnt a pointer.");
                return type_invalid;
            }
            Datatype indexType = validateExpression(ind->index);
            // TODO: is indexType a valid integer expression

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

        case ExprType_Bitwise_And:
        case ExprType_Bitwise_Or:
        case ExprType_Bitwise_Xor:
        case ExprType_Bitwise_Lshift:
        case ExprType_Bitwise_Rshift:
        case ExprType_Less:
        case ExprType_Greater:
        case ExprType_LessEquals:
        case ExprType_GreaterEquals:
        case ExprType_Equals:
        case ExprType_NotEquals:
        case ExprType_BooleanAnd:
        case ExprType_BooleanOr:
        case ExprType_Plus:
        case ExprType_Minus:
        case ExprType_Mul:
        case ExprType_Div:
        case ExprType_Mod: {
            BinaryExpression* bop = (BinaryExpression*)expr;
            Datatype leftType = validateExpression(bop->left);
            Datatype rightType = validateExpression(bop->right);

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
            validateType(&alloc->type);

            // recurse on possibe size-subexpression
            if (alloc->sizeExpr) {
                Datatype size = validateExpression(alloc->sizeExpr);
                // TODO: is this a valid integer expression?
            }

            Datatype type = alloc->type;
            type.numPointers++;
            return type;
        } break;

        case ExprType_Sizeof: {
            SizeofExpression* sof = (SizeofExpression*)expr;
            validateType(&sof->type);
            return type_uint32;
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
            validateType(&cast->castToType);

            return cast->castToType;
        } break;

        { // literals
            case ExprType_Literal_Integer: return type_ambiguousInteger;
            case ExprType_Literal_Decimal: return type_ambiguousDecimal;
            case ExprType_Literal_Char:    return type_char;
            case ExprType_Literal_String:  return type_charPointer;
            case ExprType_Literal_True:    return type_int32; // TODO: int32? maybe use another type?
            case ExprType_Literal_False:   return type_int32;
            case ExprType_Literal_Null:    return type_voidPointer;
        }

        case ExprType_Parenthesized: {
            ParenthesizedExpression* p = (ParenthesizedExpression*)expr;
            return validateExpression(p->innerExpr);
        } break;

    }

    // this should never happen
    error_node(expr, "Unknown expression type. This is a bug!");
    return type_invalid;
}

static Datatype validateExpression(Expression* expr) {
    expr->datatype = _validateExpression(expr);
    return expr->datatype;
}

static void validateStatement(Statement* sta) {
    switch (sta->statementType) {
        case Statement_FixedArray_Declaration: {
            VarDecl* decl = (VarDecl*)sta;

            // Check wheter there already is a variable with this name
            if (getVariable(decl->name).kind != Typekind_Invalid) {
                error_node(sta, "Variable \"%s\" is already declared.", get_string(decl->name));
            }

            validateType(&decl->type);

            // NOTE: decl->assignmentOrNull is used as the size expression for this fixed array
            Datatype sizetype = validateExpression(decl->assignmentOrNull);
            // TODO: is asstype a valid integer expression?

            registerVariable(decl->name, decl->type, Vartype_Variable);

        } break;
        case Statement_Declaration: {
            VarDecl* decl = (VarDecl*)sta;

            // TODO: consider whether registerVariable should do the "already declared" check
            // Check wheter there already is a variable with this name
            if (getVariable(decl->name).kind != Typekind_Invalid) {
                error_node(sta, "Variable \"%s\" is already declared.", get_string(decl->name));
            }

            validateType(&decl->type);

            if (decl->assignmentOrNull) {
                Datatype assType = validateExpression(decl->assignmentOrNull);

                if (assType.kind == Typekind_Invalid) break; // if type could not be determined then we should not continue, act as if this statement does not exist

                if (decl->type.kind == Typekind_MustBeInfered)
                    decl->type = resolveTypeAmbiguity(assType);
                else assertAssignability(decl->type, assType, decl->assignmentOrNull);
            }

            registerVariable(decl->name, decl->type, Vartype_Variable);

        } break;
        case Statement_Assignment: {
            Assignement* ass = (Assignement*)sta;

            Datatype toType = validateExpression(ass->assigneeExpr);
            Datatype fromType = validateExpression(ass->expr);

            assertAssignability(toType, fromType, ass->expr);
        } break;

        case Statement_Scope: {
            Scope* scope = (Scope*)sta;
            validateScope(scope);
        } break;

        case Statement_If: {
            IfStatement* ifsta = (IfStatement*)sta;

            // TODO: check if condition is a boolean expression
            validateExpression(ifsta->condition);

            validateStatement(ifsta->statement);

            while (ifsta->next) {
                ifsta = ifsta->next;
                if (ifsta->condition) validateExpression(ifsta->condition);
                validateStatement(ifsta->statement);
            }

        } break;
        case Statement_While: {
            WhileStatement* whileSta = (WhileStatement*)sta;
            // TODO: check if condition is a boolean expression
            validateExpression(whileSta->condition);
            validateStatement(whileSta->statement);
        } break;
        case Statement_ForIn: {
            ForInStatement* forin = (ForInStatement*)sta;

            validateExpression(forin->min_expr);
            validateExpression(forin->max_expr);

            // Check wheter there already is a variable with this name
            if (getVariable(forin->index_name).kind != Typekind_Invalid) {
                error_node(sta, "Variable \"%s\" is already declared.", get_string(forin->index_name));
            }

            registerVariable(forin->index_name, forin->index_type, Vartype_Variable);
            validateStatement(forin->statement);
            list_head(variables)->length--;

        } break;

        case Statement_Switch: {
            SwitchStatement* switchSta = (SwitchStatement*)sta;
            // TODO: check if expr is a valid type to switch on
            validateExpression(switchSta->expr);
            validateScope(switchSta->scope);
        } break;

        case Statement_LocalProc: {
            LocalProc* localproc = (LocalProc*)sta;
            validateLocalProc(localproc);
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
            if (retSta->returnExpr) type = validateExpression(retSta->returnExpr);

            if (type.kind == Typekind_Invalid) break;

            if (procedure->returnType.kind == Typekind_MustBeInfered) {
                procedure->returnType = resolveTypeAmbiguity(type);
            } else {
                if (!typeAssignable(procedure->returnType, type)) {
                    error_node(sta, "Return type missmatch in function \"%s\".", get_string(procedure->name));
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
            // TODO: is inside switch?
            CaseLabelStatement* caseLabel = (CaseLabelStatement*)sta;
            // TODO: is compiletime expr?
            validateExpression(caseLabel->expr);
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
    u32 vars_start_index = list_length(variables);
    for (u32 i = 0; i < list_length(scope->statements); i++) {
        Statement* sta = scope->statements[i];
        validateStatement(sta);
    }
    list_head(variables)->length = vars_start_index;
}

static void validateLocalProc(LocalProc* localproc) {
    validateType(&localproc->proc.returnType);
    if (localproc->proc.arguments) {
        foreach (arg, localproc->proc.arguments) validateType(&arg->type);
    }
    localproc->proc.ptr_type = ensureProcPtr(&localproc->proc);

    registerVariable(localproc->proc.name, localproc->proc.ptr_type, Vartype_Procedure);

    localproc->captures = list_create(CapturedVariable);
    current_localproc = localproc;
    capture_index = list_length(variables);
    validateProcedure(&localproc->proc);
    current_localproc = null;
}

static void validateProcedure(Procedure* proc) {
    if (proc->scope) {
        Procedure* otherproc = procedure;
        procedure = proc;

        u32 num_args = 0;
        if (proc->arguments) {
            foreach (arg, proc->arguments) registerVariable(arg->name, arg->type, Vartype_ProcArg);
            num_args = arg_count;
        }

        validateScope(proc->scope);
        list_head(variables)->length -= num_args;

        procedure = otherproc;
    }

    if (proc->returnType.kind == Typekind_MustBeInfered) {
        proc->returnType = type_void;
    }
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = list_length(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Field* field = &stru->fields[i];
        validateType(&field->type);

        // TODO: field may be an alias
        if (field->type.kind == Typekind_Struct && field->type.numPointers == 0) {
            if (stru == &current_namespace->structs[field->type.ref]) {
                error_node(field, "Struct \"%s\" self reference by value.", get_string(stru->name));
            }
        }
    }
}

static void validateGlobalVar(VarDecl* decl) {
    if (decl->assignmentOrNull) {
        Datatype assType = validateExpression(decl->assignmentOrNull);
        if (assType.kind == Typekind_Invalid) return; // if type could not be determined then we should not continue.

        if (decl->type.kind == Typekind_MustBeInfered) {
            decl->type = resolveTypeAmbiguity(assType);
        } else {
            validateType(&decl->type);
            assertAssignability(decl->type, assType, decl->assignmentOrNull);
        }

    } else {
        validateType(&decl->type);
    }
}

static void validateNamespacePrepass(Namespace* ns) {

}

static void validateNamespace(Namespace* ns) {

    current_namespace = ns;

    { // validate type aliases (except funcptrs)
        foreach (alias, current_namespace->aliases) {
            if (alias->aliasedType.kind == Typekind_ProcPtr) continue;
            validateType(&alias->aliasedType);
        }
    }


    // validate structs
    u32 struLen = list_length(current_namespace->structs);
    for (u32 i = 0; i < struLen; i++) {
        validateStruct(&current_namespace->structs[i]);
    }


    // validate constants
    foreach (cont, current_namespace->constants) {
        validateExpression(cont->expr);
    }


    // validate global variables
    u32 globLen = list_length(current_namespace->global_variables);
    for (u32 i = 0; i < globLen; i++) {
        VarDecl* decl = &current_namespace->global_variables[i];
        validateGlobalVar(&current_namespace->global_variables[i]);

        for (u32 j = i+1; j < globLen; j++) {
            if (current_namespace->global_variables[j].name == decl->name) {
                error_node(decl, "Global variable \"%s\" name conflict.", get_string(decl->name));
            }
        }
    }

    // check for function overloads
    u32 funcLen = list_length(current_namespace->procedures);
    for (u32 i = 0; i < funcLen; i++) {
        Procedure* f1 = &current_namespace->procedures[i];
        if (f1->overload) continue;

        u32 overloadCount = 1;

        // for each subsequent function
        for (u32 j = i + 1; j < funcLen; j++) {
            Procedure* f2 = &current_namespace->procedures[j];

            if (f1->name != f2->name) continue;
            // function overload detected

            f2->overload = ++overloadCount;


            // TODO: check if it is a valid overload pair
            // if (darrayLength(f1->arguments) == darrayLength(f2->arguments)) { }
        }

        if (overloadCount != 1) f1->overload = 1;
    }


    // validate the types of all function signatures (return types and argument types) before we validate the function scopes
    for (u32 i = 0; i < funcLen; i++) {
        Procedure* proc = &current_namespace->procedures[i];
        if (proc->arguments) {
            foreach (arg, proc->arguments) validateType(&arg->type);
        }

        if (proc->returnType.kind == Typekind_MustBeInfered) {
            error_temp("Return type inference not implemmented.");
            proc->returnType = type_void;
        } else {
            validateType(&proc->returnType);
        }

        proc->ptr_type = ensureProcPtr(proc);
    }

    // validate functions
    variables = list_create(Variable);
    procedure = null;
    for (u32 i = 0; i < funcLen; i++) {
        validateProcedure(&current_namespace->procedures[i]);
    }
    list_delete(variables);
}

static void validate() {

    // TODO: remove
    current_namespace = g_Codebase.namespaces[0];

    // validate funcptrs
    u32 i = 0;
    while (i < g_Codebase.procedure_types->length) {
        ProcPtr* f = getProcPtr(i);
        validateType(&f->returnType);
        if (f->argCount) {
            for (u32 i = 0; i < f->argCount; i++) validateType(&f->argTypes[i]);
        }
        i += sizeof(ProcPtr) + sizeof(Datatype) * f->argCount;
    }

    foreach (item, g_Codebase.namespaces) {
        Namespace* ns = *item;
        validateNamespace(ns);
    }
}