#include "types.h"
#include "parser.h"
#include "darray.h"

#include <stdio.h>
#include <stdarg.h>
// #include <stdlib.h> // included for malloc

typedef struct Variable {
    Identifier name;
    Datatype* type;
} Variable;

static PlangFunction* function;
static Variable* variables;
static Codeblock* currentScope;
static Statement* currentStatement;
static u32 numberOfErrors = 0;

static Datatype type_void;
static Datatype type_voidPointer;
static Datatype type_charPointer;
static Datatype type_char;

static Datatype type_ambiguousNumber = { .typeId = 0, .numPointers = 1 };
static Datatype type_int32;
static Datatype type_uint32;
static Datatype type_int64;
static Datatype type_uint64;
static Datatype type_float32;
static Datatype type_float64;

Datatype validateExpressionWithAmbiguousTypes(Expression* expr);
Datatype validateExpression(Expression* expr);
Datatype getDeclaredVariable(Identifier name);
bool typeAssignable(Datatype toType, Datatype fromType);

inline void error(char* format, ...) {
    // TODO: better line of not only statements
    u32 lineNum = 0;
    if (currentStatement) lineNum = currentStatement->nodebase.lineNumber;  
    printf("Error Ln%d: ", lineNum);

    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    printf("\n");

    numberOfErrors++;
}

inline void errorLine(u32 line, char* format, ...) {
    printf("Error Ln%d: ", line);

    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    printf("\n");

    numberOfErrors++;
}

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


    return type_null;
}

static Datatype validateVariable(VariableExpression* var) {

    Datatype type = getDeclaredVariable(var->name);
    if (typeExists(type)) return type;

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

    error("Variable \"%s\" is not declared.", getIdentifierStringValue(var->name));
    return type_null;
}

inline void validateType(Datatype type) {
    PlangType* pt = getType(type);
    if (pt->kind == Typekind_Invalid) {
        error("Type \"%s\" does not exist.", getIdentifierStringValue(pt->name));
        return;
    }

    if (pt->kind == Typekind_FuncPtr) {
        FuncPtr* f = getFuncPtr(pt->type_funcPtr);
        validateType(f->returnType);
        for (u32 i = 0; i < f->argCount; i++) {
            validateType(f->argTypes[i]);
        }
    }
}

static u32 numPointers(Datatype dt) {
    u32 nump = dt.numPointers;
    PlangType* type = getType(dt);
    if (type->kind == Typekind_Alias) {
        if (typeExists(type->type_aliasedType)) {
            nump += numPointers(type->type_aliasedType);
        }
    }

    return nump;
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
    if (typeEquals(toType, fromType)) return true;

    PlangType* to = getActualType(toType);
    u32 toPointers = numPointers(toType);

    if (isAmbiguousNumber(fromType) && toPointers == 0) {
        u32 typeId = getTypeIdOfType(to);

        if (typeId == type_int32.typeId) return true;
        else if (typeId == type_uint32.typeId) return true;
        else if (typeId == type_int64.typeId) return true;
        else if (typeId == type_uint64.typeId) return true;
        else if (typeId == type_float32.typeId) return true;
        else if (typeId == type_float64.typeId) return true;
        else return false;
    }

    PlangType* from = getActualType(fromType);
    u32 fromPointers = numPointers(fromType);

    if (toPointers == fromPointers) {

        if (to == from) return true;

        // TODO: these checks must also consider the actual type
        if (toType.typeId == type_uint64.typeId) { // when converting to ulong
            if (fromType.typeId == type_uint32.typeId) return true; // uint to ulong
        }

        if (toPointers) { // if we are a pointer to any degree 
            // void ptr casting
            if (toType.typeId == type_void.typeId || fromType.typeId == type_void.typeId) return true;

            // func ptr casting
            if (to->kind == Typekind_FuncPtr && from->kind == Typekind_FuncPtr) {
                FuncPtr* toPtr = getFuncPtr(to->type_funcPtr);
                FuncPtr* fromPtr = getFuncPtr(from->type_funcPtr);
                if (funcPtrAssignable(toPtr, fromPtr)) return true;
            }
        }
    }

    return false;
}

static void assertAssignability(Datatype toType, Datatype fromType) {
    if (!typeAssignable(toType, fromType)) {

        PlangType* to = getType(toType);
        PlangType* from = getType(fromType);

        // TODO: print correct type names. (include pointers and the proper name of function pointers) 
        // error("Type missmatch. \"%.*s\" is not compatible with \"%.*s\".",
            // from->name.length,
            // from->name.start,
            // to->name.length,
            // to->name.start);
        error("Type missmatch.");
    }
}

static Datatype validateFuncCall(FuncCall* call) {
    call->overload = 0;

    // validate passed arguments
    u32 passedArgumentsLength = call->args ? darrayLength(call->args) : 0;
    Datatype passedArguments[passedArgumentsLength];
    for (u32 i = 0; i < passedArgumentsLength; i++) passedArguments[i] = validateExpressionWithAmbiguousTypes(call->args[i]);


    if (call->funcExpr->expressionType != ExprType_Variable) goto funcptrPart;
    Identifier name = ((VariableExpression*)call->funcExpr)->name;


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
                error("Unexpected number of arguments passed to \"%s\", expected %d, but got %d.", 
                    getIdentifierStringValue(name),
                    expectedArgumentLength, passedArgumentsLength);
                return func->decl.returnType;
            }

            for (u32 i = 0; i < passedArgumentsLength; i++) {
                if (typeExists(passedArguments[i])) assertAssignability(func->decl.arguments[i].type, passedArguments[i]);
            }

            return func->decl.returnType;
        }
    }

    // function declarations
    len = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < len; i++) {
        FuncDeclaration* decl = &g_Unit->functionDeclarations[i];
        if (name != decl->name) continue;

        u32 expectedArgumentLength = decl->arguments ? darrayLength(decl->arguments) : 0;

        if (passedArgumentsLength != expectedArgumentLength) continue;

        bool compat = true;
        for (u32 i = 0; i < passedArgumentsLength; i++) {
            if (!typeExists(passedArguments[i])) continue;
            if (!typeAssignable(decl->arguments[i].type, passedArguments[i])) {compat = false; break;}
        }

        if (compat) return decl->returnType;
    }

    funcptrPart:
    Datatype calleeType = validateExpression(call->funcExpr);
    if (calleeType.typeId) {
        PlangType* type = getActualType(calleeType);
        if (type->kind == Typekind_FuncPtr) {
            call->base.expressionType = ExprType_FuncPointerCall;
            FuncPtr* funcptr = getFuncPtr(type->type_funcPtr);

            if (passedArgumentsLength != funcptr->argCount) {
                error("Unexpected number of arguments passed to function pointer, expected %d, but got %d.",
                    funcptr->argCount, passedArgumentsLength);
                return funcptr->returnType;
            }

            for (u32 i = 0; i < passedArgumentsLength; i++) {
                if (typeExists(passedArguments[i])) assertAssignability(funcptr->argTypes[i], passedArguments[i]);
            }

            return funcptr->returnType;
        }
    }


    error("Function \"%s\" was not found.", getIdentifierStringValue(name));
    return type_null;
}


static Datatype validateExpressionWithAmbiguousTypes(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Constant:
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            return validateVariable(var);
        } break;

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            Datatype datatype = validateExpression(deref->expr);
            if (!datatype.typeId) return type_null;

            if (datatype.numPointers > 1) {
                error("Attempted to dereference a %dth-degree pointer.", datatype.numPointers);
                return type_null;
            }

            deref->derefOp = datatype.numPointers ? "->" : ".";

            PlangType* type = getType(datatype);
            if (type->kind != Typekind_Struct) {
                error("Invalid dereferencing.");
                return type_null;
            }

            PlangStruct* stru = getStructByName(type->name);
            Field* field = getField(stru, deref->name);
            if (!field) {
                error("Field \"%s\" does not exist on type \"%s\".",
                        getIdentifierStringValue(deref->name),
                        getIdentifierStringValue(stru->name));
                return type_null;
            }
            return field->type;
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;
            Datatype indexedType = validateExpression(ind->indexed);
            if (indexedType.numPointers == 0) {
                error("Attempted to dereference something that isnt a pointer.");
                return type_null;
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

            if (leftType.typeId && rightType.typeId) {
                // TODO: is a valid operator operands pair?
            } else return type_null;

            return leftType;
        } break;

        case ExprType_Alloc: {
            AllocExpression* alloc = (AllocExpression*)expr;
            validateType(alloc->type);

            // recurse on possibe size-subexpression
            if (alloc->sizeExpr) {
                Datatype size = validateExpression(alloc->sizeExpr);

                if (size.typeId) {
                    // TODO: is this a valid integer expression?
                }
            }

            Datatype type = alloc->type;
            type.numPointers++;
            return type;
        } break;

        case ExprType_Sizeof: {
            SizeofExpression* sof = (SizeofExpression*)expr;
            validateType(sof->type);
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
            validateType(cast->castToType);

            return cast->castToType;
        } break;

        { // literals
            case ExprType_Literal_Integer:  return type_ambiguousNumber;
            case ExprType_Literal_Decimal:  return type_ambiguousNumber;
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

    error("Unknown expression type. This is a bug!");
    return type_null;
}

inline Datatype validateExpression(Expression* expr) {
    Datatype type = validateExpressionWithAmbiguousTypes(expr);
    if (isAmbiguousNumber(type)) return type_int32; // int32 is default number type
    return type;
}

static void validateScope(Codeblock* scope) {
    Codeblock* parentScope = currentScope;
    scope->parentScope = parentScope;
    currentScope = scope;

    u32 vars_start_index = darrayLength(variables);

    for (u32 i = 0; i < darrayLength(scope->statements); i++) {
        Statement* sta = scope->statements[i];
        currentStatement = sta;
        switch (sta->statementType) {
            case Statement_FixedArray_Declaration: {
                VarDecl* decl = (VarDecl*)sta;

                // Check wheter there already is a variable with this name
                if (getDeclaredVariable(decl->name).typeId) {
                    error("Variable \"%s\" is already declared.", getIdentifierStringValue(decl->name));
                }

                if (typeExists(decl->type)) {
                    validateType(decl->type);
                }

                Datatype asstype = validateExpression(decl->assignmentOrNull);
                // TODO: is asstype a valid integer expression?

                Variable var;
                var.name = decl->name;
                var.type = &decl->type;

                darrayAdd(variables, var);
            } break;
            case Statement_Declaration: {
                VarDecl* decl = (VarDecl*)sta;

                // Check wheter there already is a variable with this name
                if (getDeclaredVariable(decl->name).typeId) {
                    error("Variable \"%s\" is already declared.", getIdentifierStringValue(decl->name));
                }

                if (decl->assignmentOrNull) {
                    Datatype assType = validateExpressionWithAmbiguousTypes(decl->assignmentOrNull);

                    if (!typeExists(assType)) break; // if type could not be determined then we should not continue, act as if this statement does not exist 

                    if (typeExists(decl->type)) {
                        validateType(decl->type);
                        assertAssignability(decl->type, assType);
                    } else {
                        // type infer
                        decl->type = isAmbiguousNumber(assType) ? type_int32 : assType;
                    }
                } else {
                    validateType(decl->type);
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

                if (typeExists(toType) && typeExists(fromType)) {
                    assertAssignability(toType, fromType);
                }
            } break;


            case Statement_Scope: {
                validateScope(&((Scope*)sta)->codeblock);
            } break;
            case Statement_If: {
                IfStatement* ifsta = (IfStatement*)sta;

                // TODO: check if condition is a boolean expression
                validateExpression(ifsta->condition);
                validateScope(&ifsta->scope);

                while (ifsta->next) {
                    ifsta = ifsta->next;
                    if (ifsta->condition) validateExpression(ifsta->condition);
                    validateScope(&ifsta->scope);
                }

            } break;
            case Statement_While: {
                WhileStatement* whileSta = (WhileStatement*)sta;
                // TODO: check if condition is a boolean expression
                validateExpression(whileSta->condition);
                validateScope(&whileSta->scope);
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
                if (retSta->returnExpr) type = validateExpressionWithAmbiguousTypes(retSta->returnExpr);

                if (!typeExists(type)) break;

                if (typeExists(function->decl.returnType)) {
                    if (!typeAssignable(function->decl.returnType, type)) {
                        error("Return type missmatch in function \"%s\".", getIdentifierStringValue(function->decl.name));
                    }
                } else {
                    function->decl.returnType = isAmbiguousNumber(type) ? type_int32 : type;
                }


                // if (retSta->returnExpr) {
                //     PlangType* returnType = validateExpression(retSta->returnExpr);
                //     if (returnType) {
                //         if (!typeEquals(*returnType, function->decl.returnType)) {
                //             error("Return type missmatch in function \"%.*s\".", function->decl.name.length, function->decl.name.start);
                //         }
                //     }
                // } else {
                //     if (function->decl.returnType.numPointers == 0 && spanEquals(function->decl.returnType.structName, "void"));
                //     else {
                //         // TODO: proper type string in print
                //         error("Function \"%.*s\" returns %.*s, but return statement does not return any value.",
                //             function->decl.name.length, function->decl.name.start,
                //             function->decl.returnType.structName.length, function->decl.returnType.structName.start);
                //     }
                // }

            } break;

            case Statement_Goto: {
                // TODO: does label exist?
            } break;
            case Statement_Label: {
                // TODO: is label already declared?
            } break;

            case Statement_Expression: {
                StatementExpression* staExpr = (StatementExpression*)sta;
                validateExpression(staExpr->expr);
            } break;
        }
    }

    currentStatement = null;

    darrayHead(variables)->length = vars_start_index;

    currentScope = parentScope;
}

static void validateFunctionDeclaration(FuncDeclaration* decl) {

    validateType(decl->returnType);

    if (decl->arguments) {
        u32 len = darrayLength(decl->arguments);
        for (u32 i = 0; i < len; i++) validateType(decl->arguments[i].type);
    }
}

static void validateFunction(PlangFunction* func) {
    function = func;

    if (typeExists(func->decl.returnType)) {
        // TODO: factor all validateType calls out into own loop
        validateType(func->decl.returnType);
    }

    if (func->decl.arguments) {
        u32 len = darrayLength(func->decl.arguments);
        for (u32 i = 0; i < len; i++) {
            FuncArg arg = func->decl.arguments[i];
            validateType(arg.type);
        }
    }

    currentScope = null;
    validateScope(&func->scope);

    if (!typeExists(func->decl.returnType)) {
        func->decl.returnType = type_void;
    }
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = darrayLength(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Field* field = &stru->fields[i];
        validateType(field->type);
        Identifier fieldTypeName = getType(field->type)->name;

        // TODO: PlangStruct may contain its typeid so we dont have to do the spanEqualsSpan() call
        if (field->type.numPointers == 0 && fieldTypeName == stru->name) {
            errorLine(field->nodebase.lineNumber, "Struct \"%s\" self reference by value.", getIdentifierStringValue(stru->name));
        }
    }
}

static void validateGlobalVar(VarDecl* decl) {
    if (decl->assignmentOrNull) {
        Datatype assType = validateExpression(decl->assignmentOrNull);
        if (!typeExists(assType)) return; // if type could not be determined then we should not continue.

        if (typeExists(decl->type)) {
            validateType(decl->type);
            assertAssignability(decl->type, assType);
        } else {
            decl->type = isAmbiguousNumber(assType) ? type_int32 : assType;
        }

    } else {
        validateType(decl->type);
    }
}

u32 validate() {

    type_void           = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("void"))), .numPointers = 0 };
    type_voidPointer    = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("void"))), .numPointers = 1 };
    type_charPointer    = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("char"))), .numPointers = 1 };
    type_char           = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("char"))), .numPointers = 0 };
    type_int32          = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("int"))),  .numPointers = 0 };
    type_uint32         = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("uint"))),  .numPointers = 0 };
    type_int64          = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("long"))),  .numPointers = 0 };
    type_uint64         = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("ulong"))),  .numPointers = 0 };
    type_float32        = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("float"))), .numPointers = 0 };
    type_float64        = (Datatype) { .typeId = ensureTypeExistence(appendStringToStringtable(spFrom("double"))), .numPointers = 0 };

    // validate types
    u32 typeLen = darrayLength(g_Unit->types);
    for (u32 i = 0; i < typeLen; i++) {
        PlangType* type = &g_Unit->types[i];
        switch (type->kind) {
            case Typekind_Invalid: {

                // struct
                PlangStruct* stru = getStructByName(type->name);
                if (stru) {
                    type->kind = Typekind_Struct;
                    type->type_struct = stru;
                    break;
                }

                // TODO: enum

            } break;

            // case Typekind_FuncPtr: {
            //     FuncPtr* fp = getFuncPtr(type->type_funcPtr);
            // } break;

            default: break;
        }
    }

    for (u32 i = 0; i < typeLen; i++) {
        PlangType* type = &g_Unit->types[i];
        if (type->kind == Typekind_Alias) {
            // type does not exist when its an opaque type
            if (typeExists(type->type_aliasedType)) {
                validateType(type->type_aliasedType);
            }
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
                error("Global variable \"%s\" name conflict.", getIdentifierStringValue(decl->name));
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

    // validate functions
    variables = darrayCreate(Variable);
    for (u32 i = 0; i < funcLen; i++) {
        validateFunction(&g_Unit->functions[i]);
    }
    darrayDelete(variables);


    return numberOfErrors;
}