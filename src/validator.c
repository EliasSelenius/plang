#include "validator.h"

#include "types.h"
#include "parser.h"
#include "darray.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h> // included for malloc

typedef struct Variable {
    StrSpan name;
    PlangType* type;
} Variable;

static PlangFunction* function;
static Variable* variables;
static Codeblock* currentScope;
static Statement* currentStatement;
static u32 numberOfErrors = 0;

PlangType* validateExpression(Expression* expr);
PlangType* getDeclaredVariable(StrSpan name);

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

static PlangType* getDeclaredVariable(StrSpan name) {

    // look for local var
    if (variables) {
        u32 len = darrayLength(variables);
        for (u32 i = 0; i < len; i++) {
            if (spanEqualsSpan(name, variables[i].name)) {
                return variables[i].type;
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
                    return &arg->type;
                }
            }
        }
    }

    // look for global var
    u32 len = darrayLength(globalVariables);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, globalVariables[i].name)) return &globalVariables[i].type;
    }


    return null;
}

static PlangType* validateVariable(VariableExpression* var) {
    PlangType* type = getDeclaredVariable(var->name);
    if (!type) {
        error("Variable \"%.*s\" is not declared.",
            var->name.length,
            var->name.start);

        return null;
    }
    return type;
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

static PlangType* validateFuncCall(FuncCall* call) {
    
    if (call->funcExpr->expressionType != ExprType_Variable) {
        printf("Feature not implemented yet.\n");
        return null;
    }

    VariableExpression* var = (VariableExpression*)call->funcExpr;
    StrSpan name = var->name;
    call->function = getFuncDecl(name);
    if (!call->function) {
        error("Function \"%.*s\" does not exist.", name.length, name.start);
        return null;
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

        return &call->function->returnType;
    }
    
    for (u32 i = 0; i < argLen; i++) {
        PlangType* passedArgType = validateExpression(call->args[i]);
        if (passedArgType) {
            PlangType expectedArg = call->function->arguments[i].type;

            if (!typeEquals(*passedArgType, expectedArg)) {
                error("Argument type missmatch, expression of type %.*s cannot be passed to argument of type %.*s",
                    passedArgType->structName.length, passedArgType->structName.start,
                    expectedArg.structName.length, expectedArg.structName.start);
            }
        }
    }

    return &call->function->returnType;
}


static PlangType* validateExpression(Expression* expr) {
    switch (expr->expressionType) {        
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            return validateVariable(var);
        } break;

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            PlangType* type = validateExpression(deref->expr);
            if (!type) return null;
            
            if (type->numPointers > 1) {
                error("Attempted to dereference a %dth-degree pointer.", type->numPointers);
                return null;
            }

            deref->derefOp = type->numPointers ? "->" : ".";

            PlangStruct* stru = getStructByName(type->structName);
            if (stru) {
                Field* field = getField(stru, deref->name);                
                if (!field) {
                    error("Field \"%.*s\" does not exist on type \"%.*s\".",
                            field->name.length, field->name.start,
                            stru->name.length, stru->name.start);
                    return null;
                }
                return &field->type;
            } else {
                error("%.*s cannot be dereferenced.", type->structName.length, type->structName.start);
                return null;
            }

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
            PlangType* leftType = validateExpression(bop->left);
            PlangType* rightType = validateExpression(bop->right);
            
            if (leftType && rightType) {
                // TODO: is a valid operator operands pair?
            } else return null;

            return leftType;
        } break;
        
        case ExprType_Alloc: {
            AllocExpression* alloc = (AllocExpression*)expr;
            validateType(alloc->type.structName);

            // recurse on possibe size-subexpression
            if (alloc->sizeExpr) {
                PlangType* size = validateExpression(alloc->sizeExpr);

                if (size) {
                    // TODO: is this a valid integer expression?
                }
            }



            // TODO: Ugly hack here, find better memory storage for types. 
            alloc->type.numPointers++;
            return &alloc->type;

            // Or return PlangType by value perhaps? no, I have decided not to. Because we want to return null sometimes.
            // This is a memory leak!
            // PlangType* type = malloc(sizeof(PlangType));
            // *type = alloc->type;
            // type->numPointers++;
            // return type;

        } break;
        
        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            PlangType* conditionType = validateExpression(ter->condition);
            PlangType* thenType = validateExpression(ter->thenExpr);
            PlangType* elseType = validateExpression(ter->elseExpr);

            // TODO: is condition a valid boolean expression
            // TODO: does thenExpr and elseExpr have a common type? 

            return thenType;
        } break;

        case ExprType_FuncCall: {
            FuncCall* fc = (FuncCall*)expr;
            return validateFuncCall(fc);
        } break;

        { // literals
            static PlangType int32type = { .structName = { .start = "int", .length = 3 }, .numPointers = 0 };
            static PlangType charP     = { .structName = { .start = "char", .length = 4 }, .numPointers = 1 };
            static PlangType voidP     = { .structName = { .start = "void", .length = 4 }, .numPointers = 1 };

            case ExprType_Literal_Number: return &int32type;
            case ExprType_Literal_String: return &charP;
            case ExprType_Literal_Bool:   return &int32type;
            case ExprType_Literal_Null:   return &voidP;
        }

    }

    error("Unknown expression type. This is a bug!");
    return null;
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
                    PlangType* assType = validateExpression(decl->assignmentOrNull);
                    if (!assType) break; // if type could not be determined then we should not continue, act as if this statement does not exist 

                    if (decl->mustInferType) {
                        decl->type = *assType;
                    } else {
                        validateType(decl->type.structName);

                        if (!typeEquals(decl->type, *assType)) {
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

                PlangType* toType = validateExpression(ass->assigneeExpr);
                PlangType* fromType = validateExpression(ass->expr);

                if (toType && fromType) {
                    if (!typeEquals(*toType, *fromType)) {
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

                if (retSta->returnExpr) {
                    PlangType* returnType = validateExpression(retSta->returnExpr);
                    if (returnType) {
                        if (!typeEquals(*returnType, function->decl.returnType)) {
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

static void validateFunction(PlangFunction* func) {
    function = func;

    if (func->mustInferReturnType) {
        // TODO: infer return type
    } else {
        validateType(func->decl.returnType.structName);
    }

    if (func->decl.arguments) {
        u32 len = darrayLength(func->decl.arguments);
        for (u32 i = 0; i < len; i++) validateType(func->decl.arguments[i].type.structName);
    }

    currentScope = null;
    validateScope(&func->scope);
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = darrayLength(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Field* field = &stru->fields[i]; 
        validateType(field->type.structName);

        if (field->type.numPointers == 0 && spanEqualsSpan(field->type.structName, stru->name)) {
            errorLine(field->nodebase.lineNumber, "Struct \"%.*s\" self reference by value.",
                stru->name.length, stru->name.start);
        }
    }
}

static void validateGlobalVar(VarDecl* decl) {
    if (decl->assignmentOrNull) {
        PlangType* assType = validateExpression(decl->assignmentOrNull);
        if (!assType) return; // if type could not be determined then we should not continue.

        if (decl->mustInferType) {
            decl->type = *assType;
        } else {
            validateType(decl->type.structName);

            if (!typeEquals(decl->type, *assType)) {
                error("Type missmatch in global.");
            }
        }

    } else {
        validateType(decl->type.structName);
    }
}

u32 validate() {

    u32 struLen = darrayLength(structs);
    for (u32 i = 0; i < struLen; i++) {
        validateStruct(&structs[i]);
    }

    u32 globLen = darrayLength(globalVariables);
    for (u32 i = 0; i < globLen; i++) {
        VarDecl* decl = &globalVariables[i];
        validateGlobalVar(&globalVariables[i]);

        for (u32 j = i+1; j < globLen; j++) {
            if (spanEqualsSpan(globalVariables[j].name, decl->name)) {
                error("Global variable \"%.*s\" name conflict.",
                    decl->name.length, decl->name.start);
            }
        }
    }

    variables = darrayCreate(Variable);

    u32 funcLen = darrayLength(functions);
    for (u32 i = 0; i < funcLen; i++) {
        validateFunction(&functions[i]);
    }

    darrayDelete(variables);

    return numberOfErrors;
}