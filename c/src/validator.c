#include "validator.h"

#include "types.h"
#include "parser.h"
#include "darray.h"

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
            // TODO: does variable exist?
        } break;
        case ExprType_Arithmetic: {
            // TODO: is there a valid operator for these subexpressions
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
    }

    return (PlangType) {
        .structName = spFrom("err_no_type"),
        .numPointers = 0
    };
}


/*
    TODO:
*/
static void validateExpression(Expression* expr) {
    switch (expr->expressionType) {
    
        /*
        ExprType_Number,
        ExprType_String,
        ExprType_Bool,
        ExprType_Variable, // is variable declared?
        ExprType_Arithmetic, // is a valid operator operands pair?
        ExprType_Alloc, // recurse on possibe size-subexpression
        ExprType_Null
        */
    }
}


static void validateFunction(PlangFunction* func) {
    for (u32 i = 0; i < darrayLength(func->scope.statements); i++) {
        Statement* sta = &func->scope.statements[i];
        switch (sta->statementType) {
            case Statement_Declaration: {
                // TODO: is already declared?
                
                VarDecl* decl = (VarDecl*)sta->node;
                if (decl->mustInferType) {
                    if (decl->assignmentOrNull) {
                        decl->type = getExpressedType(decl->assignmentOrNull);
                    } else {
                        // TODO: error message: var must be assigned to to be infered
                    }
                } else {
                    // TODO: type missmatch? 
                }

            } break;
            case Statement_Assignment: {
                // TODO: is variable declared?
                // TODO: type missmatch?
            } break;
            case Statement_If: {
                // TODO: recurse
            } break;
            case Statement_While: {
                // TODO: recurse
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
}

void validate() {
    for (u32 i = 0; i < darrayLength(functions); i++) {
        validateFunction(&functions[i]);
    }
}

