#include "validator.h"

#include "types.h"
#include "parser.h"
#include "darray.h"

#include <stdio.h>
#include <stdarg.h>


typedef struct Variable {
    StrSpan name;
    PlangType* type;
} Variable;

static PlangFunction* function;
static Variable* variables;
static Codeblock* currentScope;
static u32 numberOfErrors = 0;

bool validateExpression(Expression* expr);


inline void error(char* format, ...) {
    printf("Error: ");

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
    u32 structsLen = darrayLength(structs);

    for (u32 i = 0; i < structsLen; i++) {
        if (spanEqualsSpan(structs[i].name, name)) return &structs[i];
    }

    return null;
}

static PlangFunction* getFunctionByName(StrSpan name) {
    u32 len = darrayLength(functions);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, functions[i].decl.name)) return &functions[i];
    }

    return null;
}

static FuncDeclaration* getFuncDecl(StrSpan name) {
    PlangFunction* func = getFunctionByName(name);
    if (func) return &func->decl;

    u32 len = darrayLength(functionDeclarations);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, functionDeclarations[i].name)) return &functionDeclarations[i];
    }

    return null;
}

inline bool typeEquals(PlangType a, PlangType b) {
    return spanEqualsSpan(a.structName, b.structName) && a.numPointers == b.numPointers;
}

static PlangType getExpressedTypeValuePath(ValuePath* value) {
    while (value->next) value = value->next;
    
    PlangType type = *value->type;

    if (value->index) type.numPointers--;
    return type;
}

static PlangType getExpressedType(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Number_Literal: {
            // TODO: different number types by literal suffix

            return (PlangType) { 
                .structName = spFrom("int"),
                .numPointers = 0
            };
        } break;
        case ExprType_String_Literal: {
            return (PlangType) {
                .structName = spFrom("char"),
                .numPointers = 1
            };
        } break;
        case ExprType_Bool_Literal: {
            return (PlangType) {
                .structName = spFrom("bool"),
                .numPointers = 0
            };
        } break;
        case ExprType_Variable: {
            ValuePath* value = expr->node;
            return getExpressedTypeValuePath(value);
        } break;
        case ExprType_Arithmetic: {

        } break;
        case ExprType_Alloc: {
            PlangType res = ((AllocExpression*)expr->node)->type;
            res.numPointers++;
            return res;
        } break;
        case ExprType_Null: {
            return (PlangType) {
                .structName = spFrom("void"),
                .numPointers = 1
            };
        } break;
        case ExprType_Ternary: {
            TernaryExpression* ter = expr->node;
            return getExpressedType(ter->thenExpr);
        } break;
        case ExprType_FuncCall: {
            FuncCall* call = expr->node;
            return call->function->returnType;
        } break;
    }

    // This is not supposed to ever hapen
    printf("getExpressedType could not determine type of expession, this is a bug!\n");
    numberOfErrors++;
    return (PlangType) {
        .structName = spFrom("err_no_type"),
        .numPointers = 0
    };
}


static PlangType* getDeclaredVariable(StrSpan name) {
    
    // look for local var
    u32 len = darrayLength(variables);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, variables[i].name)) {
            return variables[i].type;
        }
    }

    // look for func argument
    if (function->decl.arguments) {
        len = darrayLength(function->decl.arguments);
        for (u32 i = 0; i < len; i++) {
            FuncArg* arg = &function->decl.arguments[i];
            if (spanEqualsSpan(name, arg->name)) {
                return &arg->type;
            }
        }
    }

    return null;
}

static bool validateValue(ValuePath* var) {
    // is this a valid value? like a local, a function arguemnt or a global 

    PlangType* rootType = getDeclaredVariable(var->name);
    var->type = rootType;
    if (!rootType) {
        error("Variable \"%.*s\" is not declared.",
            var->name.length,
            var->name.start);

        return false;
    }

    PlangStruct* stru = getStructByName(rootType->structName);
    if (stru) {

        ValuePath* value = var;
        do {
            
            // does it use indexing? if so is it valid? and recurse on index expression
            if (value->index) {
                validateExpression(value->index);
                // TODO: is integer expression

                // TODO: is allowed to index on this type?
            }


            // is it being dereferenced? if so does the field exist?
            if (value->next) {
                Field* field = getField(stru, value->next->name);
                if (!field) {
                    error("Field \"%.*s\" does not exist on type \"%.*s\".",
                        value->next->name.length, value->next->name.start,
                        stru->name.length, stru->name.start);

                    break;
                }

                stru = getStructByName(field->type.structName);

                value->next->type = &field->type;
            }

            value = value->next;
        } while (value);

    } else {
        
    }


    return true;
}

inline void validateType(StrSpan typename) {
    // TODO: primitive types

    if (spanEquals(typename, "char"));
    else if (spanEquals(typename, "int"));
    else if (spanEquals(typename, "float"));
    else if (spanEquals(typename, "void"));
    else {
        if (!getStructByName(typename)) {
            error("Type \"%.*s\" does not exist.", typename.length, typename.start);
        } 
    }

}

static void validateFuncCall(FuncCall* call) {
    
    if (call->valuePath->next || call->valuePath->index) {
        printf("Feature not implemented yet.\n");
        return;
    }

    StrSpan name = call->valuePath->name;
    call->function = getFuncDecl(name);
    if (!call->function) {
        error("Function \"%.*s\" does not exist.", name.length, name.start);
        return;
    }

    u32 argLen = call->args ? darrayLength(call->args) : 0;
    u32 expArgLen = call->function->arguments ? darrayLength(call->function->arguments) : 0;

    if (argLen != expArgLen) {
        // TODO: print proper types with pointers
        error("Unexpected number of arguments passed to \"%.*s\", expected %d, but got %d.", 
            call->function->name.length, call->function->name.start,
            expArgLen, argLen);

        for (u32 i = 0; i < argLen; i++) {
            validateExpression(call->args[i]);
        }

        return;
    }
    
    for (u32 i = 0; i < argLen; i++) {
        if (validateExpression(call->args[i])) {
            PlangType passedArg = getExpressedType(call->args[i]);
            PlangType expectedArg = call->function->arguments[i].type;

            if (!typeEquals(passedArg, expectedArg)) {
                error("Argument type missmatch, expression of type %.*s cannot be passed to argument of type %.*s",
                    passedArg.structName.length, passedArg.structName.start,
                    expectedArg.structName.length, expectedArg.structName.start);
            }
        }
    }
}

static bool validateExpression(Expression* expr) {
    switch (expr->expressionType) {
    
        
        case ExprType_Variable: {
            ValuePath* var = expr->node;
            return validateValue(var);
        } break;
        case ExprType_Arithmetic: {
            // is a valid operator operands pair?
        } break;
        case ExprType_Alloc: {
            AllocExpression* alloc = expr->node;
            validateType(alloc->type.structName);

            // recurse on possibe size-subexpression
            if (alloc->sizeExpr) validateExpression(alloc->sizeExpr);

        } break;
        case ExprType_Ternary: {
            TernaryExpression* ter = expr->node;
            validateExpression(ter->condition);
            validateExpression(ter->thenExpr);
            validateExpression(ter->elseExpr);

            // TODO: is condition a valid boolean expression
            // TODO: does thenExpr and elseExpr have a common type? 

        } break;
        case ExprType_FuncCall: {
            validateFuncCall(expr->node);
        } break;

        // ExprType_Number_Literal,
        // ExprType_String_Literal,
        // ExprType_Bool_Literal,
        // ExprType_Null,
        default: break;
    }

    return true;
}


static void validateScope(Codeblock* scope) {
    Codeblock* parentScope = currentScope;
    scope->parentScope = parentScope;
    currentScope = scope;

    u32 vars_start_index = darrayLength(variables);

    for (u32 i = 0; i < darrayLength(scope->statements); i++) {
        Statement* sta = scope->statements[i];
        switch (sta->statementType) {
            case Statement_Declaration: {

                VarDecl* decl = (VarDecl*)sta;

                // Check wheter there already is a variable with this name
                if (getDeclaredVariable(decl->name)) {
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
                    PlangType assType;
                    if (validateExpression(decl->assignmentOrNull)) assType = getExpressedType(decl->assignmentOrNull);
                    else break; // if type could not be determined then we should not continue, act as if this statement does not exist 

                    if (decl->mustInferType) {
                        decl->type = assType;
                    } else {
                        validateType(decl->type.structName);

                        if (!typeEquals(decl->type, assType)) {
                            error("Type missmatch in declaration.");
                        }
                    }

                } else {
                    validateType(decl->type.structName);
                }


                Variable var;
                var.name = decl->name;
                var.type = &decl->type;
                
                darrayAdd(variables, var);

            } break;
            case Statement_Assignment: {
                Assignement* ass = (Assignement*)sta;
                bool assigneeValid = validateValue(ass->assignee);
                bool exprValid = validateExpression(ass->expr);
                if (assigneeValid && exprValid) {
                    PlangType asseeType = getExpressedTypeValuePath(ass->assignee);
                    PlangType exprType = getExpressedType(ass->expr);
                    if (!typeEquals(asseeType, exprType)) {
                        error("Type missmatch in assignment.");
                    }
                }

            } break;
            case Statement_If: {
                // TODO: check if condition is a boolean expression
                IfStatement* ifsta = (IfStatement*)sta;
                validateScope(&ifsta->scope);

                while (ifsta->next) {
                    ifsta = ifsta->next;
                    validateScope(&ifsta->scope);
                }

            } break;
            case Statement_While: {
                WhileStatement* whileSta = (WhileStatement*)sta;
                // TODO: check if condition is a boolean expression
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

                if (retSta->returnExpr) {
                    if (validateExpression(retSta->returnExpr)) {
                        PlangType returnType = getExpressedType(retSta->returnExpr);
                        if (!typeEquals(returnType, function->decl.returnType)) {
                            error("Return type missmatch in function \"%.*s\".", function->decl.name.length, function->decl.name.start);
                        }
                    }
                } else {
                    if (function->decl.returnType.numPointers == 0 && spanEquals(function->decl.returnType.structName, "void"));
                    else {
                        // TODO: proper type string in print
                        error("Function \"%.*s\" returns %.*s, but return statement does not return any value.",
                            function->decl.name.length, function->decl.name.start,
                            function->decl.returnType.structName.length, function->decl.returnType.structName.start);
                    }
                }

            } break;

            case Statement_FuncCall: {
                FuncCallStatement* call = (FuncCallStatement*)sta;
                validateFuncCall(&call->call);
            } break;
        }
    }

    darrayHead(variables)->length = vars_start_index;

    currentScope = parentScope;
}

static void validateFunction(PlangFunction* func) {
    function = func;

    if (func->decl.arguments) {
        u32 len = darrayLength(func->decl.arguments);
        for (u32 i = 0; i < len; i++) {
            validateType(func->decl.arguments[i].type.structName);
        }
    }

    validateType(func->decl.returnType.structName);

    currentScope = null;
    validateScope(&func->scope);
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = darrayLength(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        validateType(stru->fields[i].type.structName);
    }    
}

u32 validate() {

    u32 struLen = darrayLength(structs);
    for (u32 i = 0; i < struLen; i++) {
        validateStruct(&structs[i]);
    }

    variables = darrayCreate(Variable);

    u32 funcLen = darrayLength(functions);
    for (u32 i = 0; i < funcLen; i++) {
        validateFunction(&functions[i]);
    }

    darrayDelete(variables);

    return numberOfErrors;
}

