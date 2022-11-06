
typedef struct Variable {
    Identifier name;
    Datatype* type;
} Variable;

static PlangFunction* function;
static Variable* variables;
static Codeblock* currentScope;
static Statement* currentStatement;


static Datatype validateExpression(Expression* expr);
static Datatype getDeclaredVariable(Identifier name);
static bool typeAssignable(Datatype toType, Datatype fromType);
static void validateScope(Codeblock* scope);


static Field* getField(PlangStruct* stru, Identifier name) {
    for (u32 i = 0; i < darrayLength(stru->fields); i++) {
        if (stru->fields[i].name == name) return &stru->fields[i];
    }

    return null;
}

static PlangStruct* getStructByName(Identifier name) {
    u32 structsLen = darrayLength(g_Unit->structs);

    for (u32 i = 0; i < structsLen; i++) {
        if (g_Unit->structs[i].name == name) return &g_Unit->structs[i];
    }

    return null;
}

static PlangFunction* getFunctionByName(Identifier name) {
    u32 len = darrayLength(g_Unit->functions);
    for (u32 i = 0; i < len; i++) {
        if (name == g_Unit->functions[i].decl.name) return &g_Unit->functions[i];
    }

    return null;
}

static FuncDeclaration* getFuncDecl(Identifier name) {
    PlangFunction* func = getFunctionByName(name);
    if (func) return &func->decl;

    u32 len = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < len; i++) {
        if (name == g_Unit->functionDeclarations[i].name) return &g_Unit->functionDeclarations[i];
    }

    return null;
}

static Datatype getDeclaredVariable(Identifier name) {

    // look for local var
    if (variables) {
        u32 len = darrayLength(variables);
        for (u32 i = 0; i < len; i++) {
            if (name == variables[i].name) {
                return *variables[i].type;
            }
        }
    }

    // look for func argument
    if (function) {
        if (function->decl.arguments) {
            u32 len = darrayLength(function->decl.arguments);
            for (u32 i = 0; i < len; i++) {
                FuncArg* arg = &function->decl.arguments[i];
                if (name == arg->name) {
                    return arg->type;
                }
            }
        }
    }

    // look for global var
    u32 len = darrayLength(g_Unit->globalVariables);
    for (u32 i = 0; i < len; i++) {
        if (name == g_Unit->globalVariables[i].name) return g_Unit->globalVariables[i].type;
    }


    return type_invalid;
}

static Datatype validateVariable(VariableExpression* var) {

    Datatype type = getDeclaredVariable(var->name);
    if (type.kind != Typekind_Invalid) return type;

    FuncDeclaration* decl = getFuncDecl(var->name);
    if (decl) return ensureFuncPtrExistsFromFuncDeclaration(decl);

    // look for constants
    u32 len = darrayLength(g_Unit->constants);
    for (u32 i = 0; i < len; i++) {
        if (var->name == g_Unit->constants[i].name) {
            var->base.expressionType = ExprType_Constant;
            var->constExpr = g_Unit->constants[i].expr;
            return validateExpression(var->constExpr);
        }
    }

    error_node(var, "Variable \"%s\" is not declared.", getIdentifierStringValue(var->name));
    return type_invalid;
}

static void validateType(Datatype* type) {
    switch (type->kind) {

        case Typekind_Undecided: {

            foreach (stru, g_Unit->structs) {
                if (stru->name == type->ref) {
                    type->kind = Typekind_Struct;
                    type->ref = stru_index;
                    return;
                }
            }

            // TODO: enums

            foreach (alias, g_Unit->aliases) {
                if (alias->name == type->ref) {
                    type->kind = Typekind_Alias;
                    type->ref = alias_index;
                    return;
                }
            }

            foreach (optype, g_Unit->opaqueTypes) {
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

static bool funcPtrAssignable(FuncPtr* to, FuncPtr* from) {
    if (to->argCount != from->argCount) return false;

    if (!typeAssignable(to->returnType, from->returnType)) return false;

    for (u32 i = 0; i < to->argCount; i++) {
        if (!typeAssignable(to->argTypes[i], from->argTypes[i])) return false;
    }

    return true;
}

static bool typeAssignable(Datatype toType, Datatype fromType) {
    if (toType.kind == Typekind_Alias) {
        Datatype alias = g_Unit->aliases[toType.ref].aliasedType;
        alias.numPointers += toType.numPointers;
        return typeAssignable(alias, fromType);
    } else if (fromType.kind == Typekind_Alias) {
        Datatype alias = g_Unit->aliases[fromType.ref].aliasedType;
        alias.numPointers += fromType.numPointers;
        return typeAssignable(toType, alias);
    }

    if (fromType.kind == Typekind_Invalid) return true;
    if (toType.kind == Typekind_Invalid) return true;

    if (typeEquals(toType, fromType)) return true;

    if (toType.numPointers != fromType.numPointers) return false;
    u32 numPointers = toType.numPointers;

    if (numPointers) { // if we are a pointer of any degree

        if (toType.kind == Typekind_void || fromType.kind == Typekind_void) return true;

        if (toType.kind == Typekind_FuncPtr && fromType.kind == Typekind_FuncPtr) {
            FuncPtr* toPtr = getFuncPtr(toType.ref);
            FuncPtr* fromPtr = getFuncPtr(fromType.ref);
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
            toType.kind == Typekind_float64) return true;

        if (isIntegralType(toType))
            if (rangein(getNumericDomain(fromType), getNumericDomain(toType)))
                return true;

        return false;
    }

    if (fromType.kind == Typekind_float32 && toType.kind == Typekind_float64) return true; else return false;

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

        case Typekind_Struct: typename = getIdentifierStringValue(g_Unit->structs[type.ref].name); break;
        case Typekind_Enum: typename = null; break;
        case Typekind_Alias: typename = getIdentifierStringValue(g_Unit->aliases[type.ref].name); break;
        case Typekind_Opaque: typename = getIdentifierStringValue(type.ref); break;
        case Typekind_FuncPtr: typename = "SomeFuncPtr"; break;

    }

    snprintf(buffer, size, "%s%.*s", typename, type.numPointers, "**********");
}

static void assertAssignability(Datatype toType, Datatype fromType) {
    if (!typeAssignable(toType, fromType)) {

        char toTypeName[50];
        char fromTypeName[50];
        constructTypename(toTypeName, sizeof(toTypeName), toType);
        constructTypename(fromTypeName, sizeof(fromTypeName), fromType);

        error_temp("Type missmatch. \"%s\" is not assignable to \"%s\".", fromTypeName, toTypeName);
    }
}

static Datatype validateFuncCall(FuncCall* call) {
    call->overload = 0;

    // validate passed arguments
    u32 passedArgumentsLength = call->args ? darrayLength(call->args) : 0;
    Datatype passedArguments[passedArgumentsLength]; // TODO: we store the datatype in Expression now, so we can change this code.
    for (u32 i = 0; i < passedArgumentsLength; i++) passedArguments[i] = validateExpression(call->args[i]);


    if (call->funcExpr->expressionType != ExprType_Variable) goto funcptrPart;
    Identifier name = ((VariableExpression*)call->funcExpr)->name;

    if (name == builtin_print_name) return type_void;

    // look for function with possible overloads
    u32 len = darrayLength(g_Unit->functions);
    for (u32 i = 0; i < len; i++) {
        PlangFunction* func = &g_Unit->functions[i];
        if (name != func->decl.name) continue;

        u32 expectedArgumentLength = func->decl.arguments ? darrayLength(func->decl.arguments) : 0;

        if (func->overload) {
            if (passedArgumentsLength != expectedArgumentLength) continue;

            bool compat = true;
            for (u32 i = 0; i < passedArgumentsLength; i++) {
                if (!typeAssignable(func->decl.arguments[i].type, passedArguments[i])) {compat = false; break;}
            }

            if (compat) {
                call->overload = func->overload;
                return func->decl.returnType;
            } else continue;

        } else {
            if (passedArgumentsLength != expectedArgumentLength) {
                error_temp("Unexpected number of arguments passed to \"%s\", expected %d, but got %d.",
                    getIdentifierStringValue(name),
                    expectedArgumentLength, passedArgumentsLength);
                return func->decl.returnType;
            }

            for (u32 i = 0; i < passedArgumentsLength; i++) assertAssignability(func->decl.arguments[i].type, passedArguments[i]);
            return func->decl.returnType;
        }
    }

    // function declarations
    len = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < len; i++) {
        FuncDeclaration* decl = &g_Unit->functionDeclarations[i];
        if (name != decl->name) continue;

        u32 expectedArgumentLength = decl->arguments ? darrayLength(decl->arguments) : 0;

        if (passedArgumentsLength != expectedArgumentLength) {
            error_temp("Unexpected number of arguments passed to \"%s\", expected %d, but got %d.",
                getIdentifierStringValue(name),
                expectedArgumentLength, passedArgumentsLength);
            return decl->returnType;
        }

        for (u32 i = 0; i < passedArgumentsLength; i++) assertAssignability(decl->arguments[i].type, passedArguments[i]);
        return decl->returnType;
    }

    funcptrPart:
    Datatype calleeType = dealiasType(validateExpression(call->funcExpr));
    if (calleeType.kind == Typekind_FuncPtr) { // what if it is a double pointer? Then this shouldnt work.
        call->base.expressionType = ExprType_FuncPointerCall;
        FuncPtr* funcptr = getFuncPtr(calleeType.ref);

        if (passedArgumentsLength != funcptr->argCount) {
            error_temp("Unexpected number of arguments passed to function pointer, expected %d, but got %d.",
                funcptr->argCount, passedArgumentsLength);
            return funcptr->returnType;
        }

        for (u32 i = 0; i < passedArgumentsLength; i++) assertAssignability(funcptr->argTypes[i], passedArguments[i]);
        return funcptr->returnType;
    }


    error_temp("Function \"%s\" was not found.", getIdentifierStringValue(name));
    return type_invalid;
}

static Datatype _validateExpression(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Constant:
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            return validateVariable(var);
        } break;

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            Datatype datatype = validateExpression(deref->expr);
            if (datatype.kind == Typekind_Invalid) return type_invalid;

            if (datatype.numPointers > 1) {
                error_node(expr, "Attempted to dereference a %dth-degree pointer.", datatype.numPointers);
                return type_invalid;
            }

            deref->derefOp = datatype.numPointers ? "->" : ".";

            if (datatype.kind != Typekind_Struct) {
                error_node(expr, "Invalid dereferencing.");
                return type_invalid;
            }

            PlangStruct* stru = &g_Unit->structs[datatype.ref];
            Field* field = getField(stru, deref->name);
            if (!field) {
                error_node(expr, "Field \"%s\" does not exist on type \"%s\".",
                        getIdentifierStringValue(deref->name),
                        getIdentifierStringValue(stru->name));
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

            return leftType;
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

        case ExprType_FuncPointerCall:
        case ExprType_FuncCall: {
            FuncCall* fc = (FuncCall*)expr;
            return validateFuncCall(fc);
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
            if (getDeclaredVariable(decl->name).kind != Typekind_Invalid) {
                error_node(sta, "Variable \"%s\" is already declared.", getIdentifierStringValue(decl->name));
            }

            validateType(&decl->type);

            // NOTE: decl->assignmentOrNull is used as the size expression for this fixed array
            Datatype sizetype = validateExpression(decl->assignmentOrNull);
            // TODO: is asstype a valid integer expression?

            Variable var;
            var.name = decl->name;
            var.type = &decl->type;

            darrayAdd(variables, var);
        } break;
        case Statement_Declaration: {
            VarDecl* decl = (VarDecl*)sta;

            // Check wheter there already is a variable with this name
            if (getDeclaredVariable(decl->name).kind != Typekind_Invalid) {
                error_node(sta, "Variable \"%s\" is already declared.", getIdentifierStringValue(decl->name));
            }

            validateType(&decl->type);

            if (decl->assignmentOrNull) {
                Datatype assType = validateExpression(decl->assignmentOrNull);

                if (assType.kind == Typekind_Invalid) break; // if type could not be determined then we should not continue, act as if this statement does not exist 

                if (decl->type.kind == Typekind_MustBeInfered)
                    decl->type = resolveTypeAmbiguity(assType);
                else assertAssignability(decl->type, assType);
            }

            Variable var;
            var.name = decl->name;
            var.type = &decl->type;

            darrayAdd(variables, var);

        } break;
        case Statement_Assignment: {
            Assignement* ass = (Assignement*)sta;

            Datatype toType = validateExpression(ass->assigneeExpr);
            Datatype fromType = validateExpression(ass->expr);

            assertAssignability(toType, fromType);
        } break;


        case Statement_Scope: {
            validateScope(&((Scope*)sta)->codeblock);
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

        case Statement_Switch: {
            SwitchStatement* switchSta = (SwitchStatement*)sta;
            // TODO: check if expr is a valid type to switch on
            validateExpression(switchSta->expr);
            validateScope(&switchSta->scope);
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

            if (function->decl.returnType.kind == Typekind_MustBeInfered) {
                function->decl.returnType = resolveTypeAmbiguity(type);
            } else {
                if (!typeAssignable(function->decl.returnType, type)) {
                    error_node(sta, "Return type missmatch in function \"%s\".", getIdentifierStringValue(function->decl.name));
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

static void validateScope(Codeblock* scope) {
    Codeblock* parentScope = currentScope;
    scope->parentScope = parentScope;
    currentScope = scope;

    u32 vars_start_index = darrayLength(variables);

    for (u32 i = 0; i < darrayLength(scope->statements); i++) {
        Statement* sta = scope->statements[i];
        currentStatement = sta;
        validateStatement(sta);
    }

    currentStatement = null;
    darrayHead(variables)->length = vars_start_index;
    currentScope = parentScope;
}

static void validateFunctionDeclaration(FuncDeclaration* decl) {

    validateType(&decl->returnType);

    if (decl->arguments) {
        u32 len = darrayLength(decl->arguments);
        for (u32 i = 0; i < len; i++) validateType(&decl->arguments[i].type);
    }
}

static void validateFunction(PlangFunction* func) {
    function = func;

    // validateFunctionDeclaration(&func->decl);

    currentScope = null;
    validateScope(&func->scope);

    if (func->decl.returnType.kind == Typekind_MustBeInfered) {
        func->decl.returnType = type_void;
    }
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = darrayLength(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Field* field = &stru->fields[i];
        validateType(&field->type);

        // TODO: field may be an alias
        if (field->type.kind == Typekind_Struct && field->type.numPointers == 0) {
            if (stru == &g_Unit->structs[field->type.ref]) {
                error_node(field, "Struct \"%s\" self reference by value.", getIdentifierStringValue(stru->name));
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
            assertAssignability(decl->type, assType);
        }

    } else {
        validateType(&decl->type);
    }
}

static void validate() {

    // validate funcptrs
    u32 i = 0;
    while (i < g_Unit->funcPtrTypes->length) {
        FuncPtr* f = getFuncPtr(i);
        validateType(&f->returnType);
        if (f->argCount) {
            for (u32 i = 0; i < f->argCount; i++) validateType(&f->argTypes[i]);
        }
        i += sizeof(FuncPtr) + sizeof(Datatype) * f->argCount;
    }

    { // validate type aliases (except funcptrs)
        foreach (alias, g_Unit->aliases) {
            if (alias->aliasedType.kind == Typekind_FuncPtr) continue;
            validateType(&alias->aliasedType);
        }
    }


    // validate structs
    u32 struLen = darrayLength(g_Unit->structs);
    for (u32 i = 0; i < struLen; i++) {
        validateStruct(&g_Unit->structs[i]);
    }

    // validate global variables
    u32 globLen = darrayLength(g_Unit->globalVariables);
    for (u32 i = 0; i < globLen; i++) {
        VarDecl* decl = &g_Unit->globalVariables[i];
        validateGlobalVar(&g_Unit->globalVariables[i]);

        for (u32 j = i+1; j < globLen; j++) {
            if (g_Unit->globalVariables[j].name == decl->name) {
                error_node(decl, "Global variable \"%s\" name conflict.", getIdentifierStringValue(decl->name));
            }
        }
    }

    // validate func declarations
    u32 funcDeclLen = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < funcDeclLen; i++) {
        validateFunctionDeclaration(&g_Unit->functionDeclarations[i]);
    }

    // check for function overloads
    u32 funcLen = darrayLength(g_Unit->functions);
    for (u32 i = 0; i < funcLen; i++) {
        PlangFunction* f1 = &g_Unit->functions[i];
        if (f1->overload) continue;

        u32 overloadCount = 1;

        // for each subsequent function
        for (u32 j = i + 1; j < funcLen; j++) {
            PlangFunction* f2 = &g_Unit->functions[j];

            if (f1->decl.name != f2->decl.name) continue;
            // function overload detected

            f2->overload = ++overloadCount;


            // TODO: check if it is a valid overload pair
            // if (darrayLength(f1->arguments) == darrayLength(f2->arguments)) { }
        }

        if (overloadCount != 1) f1->overload = 1;
    }


    // validate the types of all function signatures (return types and argument types) before we validate the function scopes
    for (u32 i = 0; i < funcLen; i++) {
        PlangFunction* func = &g_Unit->functions[i];
        if (func->decl.arguments) {
            foreach (arg, func->decl.arguments) validateType(&arg->type);
        }

        if (func->decl.returnType.kind == Typekind_MustBeInfered) {
            error_temp("Return type inference not implemmented.");
            func->decl.returnType = type_void;
        } else {
            validateType(&func->decl.returnType);
        }
    }

    // validate functions
    variables = darrayCreate(Variable);
    for (u32 i = 0; i < funcLen; i++) {
        validateFunction(&g_Unit->functions[i]);
    }
    darrayDelete(variables);
}