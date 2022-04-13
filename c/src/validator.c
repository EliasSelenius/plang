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
        if (spanEqualsSpan(name, functions[i].name)) return &functions[i];
    }

    return null;
}

inline bool typeEquals(PlangType a, PlangType b) {
    return spanEqualsSpan(a.structName, b.structName) && a.numPointers == b.numPointers;
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
            while (value->next) value = value->next;
            
            PlangType type = *value->type;

            if (value->index) type.numPointers--;
            return type;

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
    if (function->arguments) {
        len = darrayLength(function->arguments);
        for (u32 i = 0; i < len; i++) {
            FuncArg* arg = &function->arguments[i];
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
    if (!stru) {
        return false;
    }

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

    return true;
}

inline void validateType(StrSpan typename) {
    // TODO: primitive types
    if (!getStructByName(typename)) {
        error("Type \"%.*s\" does not exist.", typename.length, typename.start);
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
        Statement* sta = &scope->statements[i];
        switch (sta->statementType) {
            case Statement_Declaration: {

                VarDecl* decl = (VarDecl*)sta->node;

                // Check wheter there already is a variable with this name
                if (getDeclaredVariable(decl->name)) {
                    error("Variable \"%.*s\" is already declared.", 
                        decl->name.length,
                        decl->name.start);
                }
                        
                if (decl->mustInferType) {
                    if (decl->assignmentOrNull) {
                        if (validateExpression(decl->assignmentOrNull)) {
                            decl->type = getExpressedType(decl->assignmentOrNull);
                        }
                    } else {
                        error("Variable \"%.*s\" must be assigned to, to be type inferred.", decl->name.length, decl->name.start);
                    }
                } else {
                    validateType(decl->type.structName);
                    // TODO: type missmatch? 
                }

                Variable var;
                var.name = decl->name;
                var.type = &decl->type;
                
                darrayAdd(variables, var);

            } break;
            case Statement_Assignment: {
                Assignement* ass = sta->node;
                validateValue(ass->assignee);
                validateExpression(ass->expr);

                // TODO: type missmatch?
            } break;
            case Statement_If: {
                // TODO: check if condition is a boolean expression
                IfStatement* ifsta = sta->node;
                validateScope(&ifsta->scope);

                while (ifsta->next) {
                    ifsta = ifsta->next;
                    validateScope(&ifsta->scope);
                }

            } break;
            case Statement_While: {
                WhileStatement* whileSta = sta->node;
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
                // TODO: void functions
                Expression* returnExpr = sta->node;
                // if (returnExpr)
                if (validateExpression(returnExpr)) {
                    PlangType returnType = getExpressedType(returnExpr);
                    if (!typeEquals(returnType, function->returnType)) {
                        error("Return type missmatch in function \"%.*s\".", function->name.length, function->name.start);
                    }
                }

            } break;

            case Statement_FuncCall: {
                FuncCall* call = sta->node;

                if (call->valuePath->next || call->valuePath->index) {
                    printf("Feature not implemented yet.\n");
                    break;
                }

                StrSpan name = call->valuePath->name;
                call->function = getFunctionByName(name);
                if (!call->function) {
                    error("Function \"%.*s\" does not exist.", name.length, name.start);
                    break;
                }

                
            } break;
        }
    }

    darrayHead(variables)->length = vars_start_index;

    currentScope = parentScope;
}

static void validateFunction(PlangFunction* func) {
    function = func;
    currentScope = null;
    validateScope(&func->scope);
}

u32 validate() {

    variables = darrayCreate(Variable);

    for (u32 i = 0; i < darrayLength(functions); i++) {
        validateFunction(&functions[i]);
    }

    darrayDelete(variables);

    return numberOfErrors;
}

