#include "validator.h"

#include "types.h"
#include "parser.h"
#include "darray.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h> // included for malloc

typedef struct Variable {
    StrSpan name;
    Datatype* type;
} Variable;

static PlangFunction* function;
static Variable* variables;
static Codeblock* currentScope;
static Statement* currentStatement;
static u32 numberOfErrors = 0;

static Datatype type_void;
static Datatype type_voidPointer;
static Datatype type_int32;
static Datatype type_float32;
static Datatype type_charPointer;

Datatype validateExpression(Expression* expr);
Datatype getDeclaredVariable(StrSpan name);

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

static Field* getField(PlangStruct* stru, StrSpan name) {
    for (u32 i = 0; i < darrayLength(stru->fields); i++) {
        if (spanEqualsSpan(stru->fields[i].name, name)) return &stru->fields[i];
    }

    return null;
}

static PlangStruct* getStructByName(StrSpan name) {
    u32 structsLen = darrayLength(g_Unit->structs);

    for (u32 i = 0; i < structsLen; i++) {
        if (spanEqualsSpan(g_Unit->structs[i].name, name)) return &g_Unit->structs[i];
    }

    return null;
}

static PlangFunction* getFunctionByName(StrSpan name) {
    u32 len = darrayLength(g_Unit->functions);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, g_Unit->functions[i].decl.name)) return &g_Unit->functions[i];
    }

    return null;
}

static FuncDeclaration* getFuncDecl(StrSpan name) {
    PlangFunction* func = getFunctionByName(name);
    if (func) return &func->decl;

    u32 len = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, g_Unit->functionDeclarations[i].name)) return &g_Unit->functionDeclarations[i];
    }

    return null;
}

static Datatype getDeclaredVariable(StrSpan name) {

    // look for local var
    if (variables) {
        u32 len = darrayLength(variables);
        for (u32 i = 0; i < len; i++) {
            if (spanEqualsSpan(name, variables[i].name)) {
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
                if (spanEqualsSpan(name, arg->name)) {
                    return arg->type;
                }
            }
        }
    }

    // look for global var
    u32 len = darrayLength(g_Unit->globalVariables);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, g_Unit->globalVariables[i].name)) return g_Unit->globalVariables[i].type;
    }

    return type_null;
}

static Datatype validateVariable(VariableExpression* var) {
    
    Datatype type = getDeclaredVariable(var->name);
    if (type.typeId) return type;

    FuncDeclaration* decl = getFuncDecl(var->name);
    if (decl) return ensureFuncPtrExistsFromFuncDeclaration(decl);


    error("Variable \"%.*s\" is not declared.",
        var->name.length,
        var->name.start);
    return type_null;
}

inline void validateType(Datatype type) {
    PlangType* pt = getType(type);
    if (pt->kind != Typekind_Invalid) return;

    error("Type \"%.*s\" does not exist.", pt->name.length, pt->name.start);
}


static Datatype validateFuncCall(FuncCall* call) {
    
    if (call->funcExpr->expressionType != ExprType_Variable) {
        printf("Feature not implemented yet.\n");
        return type_null;
    }

    VariableExpression* var = (VariableExpression*)call->funcExpr;
    StrSpan name = var->name;
    call->functionName = name;
    FuncDeclaration* func = getFuncDecl(name);
    if (!func) {

        // calling function pointer? 
        Datatype varType = getDeclaredVariable(name);
        if (varType.typeId) {
            PlangType* type = getType(varType);
            if (type->kind == Typekind_FuncPtr) {
                call->base.expressionType = ExprType_FuncPointerCall;
                FuncPtr* funcPtr = getFuncPtr(type->type_funcPtr);

                u32 argLen = call->args ? darrayLength(call->args) : 0;
                for (u32 i = 0; i < argLen; i++) {
                    Datatype t = validateExpression(call->args[i]);
                }

                return funcPtr->returnType;
            }
        }

        error("Function \"%.*s\" does not exist.", name.length, name.start);
        return type_null;
    }

    u32 argLen = call->args ? darrayLength(call->args) : 0;
    u32 expArgLen = func->arguments ? darrayLength(func->arguments) : 0;

    if (argLen != expArgLen) {
        // TODO: print proper types with pointers
        error("Unexpected number of arguments passed to \"%.*s\", expected %d, but got %d.", 
            func->name.length, func->name.start,
            expArgLen, argLen);

        for (u32 i = 0; i < argLen; i++) {
            validateExpression(call->args[i]);
        }

        return func->returnType;
    }
    
    for (u32 i = 0; i < argLen; i++) {
        Datatype passedArgType = validateExpression(call->args[i]);
        if (passedArgType.typeId) {
            Datatype expectedArg = func->arguments[i].type;

            if (!typeEquals(passedArgType, expectedArg)) {
                StrSpan passedName = getType(passedArgType)->name;
                StrSpan expectedName = getType(expectedArg)->name;
                error("Argument type missmatch, expression of type %.*s cannot be passed to argument of type %.*s",
                    passedName.length, passedName.start,
                    expectedName.length, expectedName.start);
            }
        }
    }

    return func->returnType;
}


static Datatype validateExpression(Expression* expr) {
    switch (expr->expressionType) {        
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            return validateVariable(var);
        } break;

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            Datatype type = validateExpression(deref->expr);
            if (!type.typeId) return type_null;
            
            if (type.numPointers > 1) {
                error("Attempted to dereference a %dth-degree pointer.", type.numPointers);
                return type_null;
            }

            deref->derefOp = type.numPointers ? "->" : ".";

            StrSpan name = getType(type)->name;
            PlangStruct* stru = getStructByName(name);
            if (stru) {
                Field* field = getField(stru, deref->name);                
                if (!field) {
                    error("Field \"%.*s\" does not exist on type \"%.*s\".",
                            field->name.length, field->name.start,
                            stru->name.length, stru->name.start);
                    return type_null;
                }
                return field->type;
            } else {
                error("%.*s cannot be dereferenced.", name.length, name.start);
                return type_null;
            }

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
        case ExprType_BooleanOr:
        case ExprType_Plus:
        case ExprType_Minus:
        case ExprType_Mul:
        case ExprType_Div: {
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

        { // literals
            case ExprType_Literal_Integer: return type_int32;
            case ExprType_Literal_Decimal: return type_float32;
            case ExprType_Literal_String:  return type_charPointer;
            case ExprType_Literal_Bool:    return type_int32;
            case ExprType_Literal_Null:    return type_voidPointer;
        }

    }

    error("Unknown expression type. This is a bug!");
    return type_null;
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
            case Statement_Declaration: {
                VarDecl* decl = (VarDecl*)sta;

                // Check wheter there already is a variable with this name
                if (getDeclaredVariable(decl->name).typeId) {
                    error("Variable \"%.*s\" is already declared.", 
                        decl->name.length,
                        decl->name.start);
                }
                
                /*
                    assign

                    yes       o     o
                    no        x     o

                    infer    yes    no      
                */

                if (decl->assignmentOrNull) {
                    Datatype assType = validateExpression(decl->assignmentOrNull);
                    if (!assType.typeId) break; // if type could not be determined then we should not continue, act as if this statement does not exist 

                    if (typeMustBeInfered(decl->type)) {
                        decl->type = assType;
                    } else {
                        validateType(decl->type);

                        if (!typeEquals(decl->type, assType)) {
                            error("Type missmatch in declaration.");
                        }
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

                if (toType.typeId && fromType.typeId) {
                    if (!typeEquals(toType, fromType)) {
                        error("Type missmatch in assignment.");
                    }
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
                if (retSta->returnExpr) type = validateExpression(retSta->returnExpr);

                if (!type.typeId) break;

                if (typeMustBeInfered(function->decl.returnType)) {
                    function->decl.returnType = type;
                } else {
                    if (!typeEquals(type, function->decl.returnType)) {
                        error("Return type missmatch in function \"%.*s\".", function->decl.name.length, function->decl.name.start);
                    }
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

    if (!typeMustBeInfered(func->decl.returnType)) {
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

    if (typeMustBeInfered(func->decl.returnType)) {
        func->decl.returnType = type_void;
    }
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = darrayLength(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Field* field = &stru->fields[i];
        validateType(field->type);
        StrSpan fieldTypeName = getType(field->type)->name;

        // TODO: PlangStruct may contain its typeid so we dont have to do the spanEqualsSpan() call
        if (field->type.numPointers == 0 && spanEqualsSpan(fieldTypeName, stru->name)) {
            errorLine(field->nodebase.lineNumber, "Struct \"%.*s\" self reference by value.",
                stru->name.length, stru->name.start);
        }
    }
}

static void validateGlobalVar(VarDecl* decl) {
    if (decl->assignmentOrNull) {
        Datatype assType = validateExpression(decl->assignmentOrNull);
        if (!assType.typeId) return; // if type could not be determined then we should not continue.

        if (typeMustBeInfered(decl->type)) {
            decl->type = assType;
        } else {
            validateType(decl->type);

            if (!typeEquals(decl->type, assType)) {
                error("Type missmatch in global.");
            }
        }

    } else {
        validateType(decl->type);
    }
}

u32 validate() {

    type_void           = (Datatype) { .typeId = ensureTypeExistence(spFrom("void")), .numPointers = 0 };
    type_voidPointer    = (Datatype) { .typeId = ensureTypeExistence(spFrom("void")), .numPointers = 1 };
    type_int32          = (Datatype) { .typeId = ensureTypeExistence(spFrom("int")),  .numPointers = 0 };
    type_float32        = (Datatype) { .typeId = ensureTypeExistence(spFrom("float")), .numPointers = 0 };
    type_charPointer    = (Datatype) { .typeId = ensureTypeExistence(spFrom("char")), .numPointers = 1 };


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
            if (spanEqualsSpan(g_Unit->globalVariables[j].name, decl->name)) {
                error("Global variable \"%.*s\" name conflict.",
                    decl->name.length, decl->name.start);
            }
        }
    }

    // validate func declarations
    u32 funcDeclLen = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < funcDeclLen; i++) {
        validateFunctionDeclaration(&g_Unit->functionDeclarations[i]);
    }

    // validate functions
    variables = darrayCreate(Variable);
    u32 funcLen = darrayLength(g_Unit->functions);
    for (u32 i = 0; i < funcLen; i++) {
        validateFunction(&g_Unit->functions[i]);
    }
    darrayDelete(variables);

    return numberOfErrors;
}