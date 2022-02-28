#include "validator.h"

#include "types.h"
#include "parser.h"
#include "darray.h"

#include <stdio.h>

void validateExpression(Expression* expr);

static Field* getField(PlangStruct* stru, StrSpan name) {
    for (u32 i = 0; i < darrayLength(stru->fields); i++) {
        if (spanEqualsSpan(stru->fields[i].name, name)) return &stru->fields[i];
    }

    return 0;
}

static PlangStruct* getStructByName(StrSpan name) {
    u32 structsLen = darrayLength(structs);

    for (u32 i = 0; i < structsLen; i++) {
        if (spanEqualsSpan(structs[i].name, name)) return &structs[i];
    }

    return NULL;
}

static PlangType getExpressedType(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Number: {
            // TODO: different number types by literal suffix

            return (PlangType) { 
                .structName = spFrom("int"),
                .numPointers = 0
            };
        } break;
        case ExprType_String: {
            return (PlangType) {
                .structName = spFrom("char"),
                .numPointers = 1
            };
        } break;
        case ExprType_Bool: {
            return (PlangType) {
                .structName = spFrom("bool"),
                .numPointers = 0
            };
        } break;
        case ExprType_Variable: {
            // TODO: fetch type of var.
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

    return (PlangType) {
        .structName = spFrom("err_no_type"),
        .numPointers = 0
    };
}

static void validateValue(ValuePath* var) {
    // is this a valid value? like a local, a function arguemnt or a global 


    ValuePath* value = var;

    PlangStruct* stru = getStructByName(spFrom("Foo"));
    
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
                printf("Error. Field \"%.*s\" does not exist on type \"%.*s\".\n",
                    value->next->name.length, value->next->name.start,
                    stru->name.length, stru->name.start);
                break;
            }
            stru = getStructByName(field->type.structName);
        }

        value = value->next;
    } while (value);

}

inline void validateType(StrSpan typename) {
    // TODO: primitive types
    if (!getStructByName(typename)) {
        printf("Error. type \"%.*s\" does not exist.\n", typename.length, typename.start);
    } 
}

static void validateExpression(Expression* expr) {
    switch (expr->expressionType) {
    
        
        case ExprType_Variable: {
            ValuePath* var = expr->node;
            validateValue(var);
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

        // ExprType_Number,
        // ExprType_String,
        // ExprType_Bool,
        // ExprType_Null,
        default: break;
    }
}

static Codeblock* currentScope;
static void validateScope(Codeblock* scope) {
    Codeblock* parentScope = currentScope;
    scope->parentScope = parentScope;
    currentScope = scope;

    for (u32 i = 0; i < darrayLength(scope->statements); i++) {
        Statement* sta = &scope->statements[i];
        switch (sta->statementType) {
            case Statement_Declaration: {
                // TODO: is already declared?
                
                VarDecl* decl = (VarDecl*)sta->node;
                if (decl->mustInferType) {
                    if (decl->assignmentOrNull) {
                        validateExpression(decl->assignmentOrNull);
                        decl->type = getExpressedType(decl->assignmentOrNull);
                    } else {
                        // TODO: error message: var must be assigned to to be infered
                    }
                } else {
                    validateType(decl->type.structName);
                    // TODO: type missmatch? 
                }

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
                // TODO: return type missmatch?
            } break;
        }
    }

    currentScope = parentScope;
}

static void validateFunction(PlangFunction* func) {
    currentScope = 0;
    validateScope(&func->scope);
}

void validate() {
    for (u32 i = 0; i < darrayLength(functions); i++) {
        validateFunction(&functions[i]);
    }
}

