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

static PlangType getExpressedTypeValuePath(ValuePath* value) {
    while (value->next) value = value->next;
    
    PlangType type = *value->type;

    if (value->index) type.numPointers--;
    return type;
}

static PlangType getExpressedType(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Literal_Number: {
            // TODO: different number types by literal suffix

            return (PlangType) { 
                .structName = spFrom("int"),
                .numPointers = 0
            };
        } break;
        case ExprType_Literal_String: {
            return (PlangType) {
                .structName = spFrom("char"),
                .numPointers = 1
            };
        } break;
        case ExprType_Literal_Bool: {
            return (PlangType) {
                .structName = spFrom("int"),
                .numPointers = 0
            };
        } break;
        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            return *getDeclaredVariable(var->name);
            // ValuePath* value = ((ExpressionProxy*)expr)->node;
            // return getExpressedTypeValuePath(value);
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
            return (PlangType) { 
                .structName = spFrom("int"),
                .numPointers = 0
            };
        } break;
        case ExprType_Alloc: {
            PlangType res = ((AllocExpression*)expr)->type;
            res.numPointers++;
            return res;
        } break;
        case ExprType_Literal_Null: {
            return (PlangType) {
                .structName = spFrom("void"),
                .numPointers = 1
            };
        } break;
        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            return getExpressedType(ter->thenExpr);
        } break;
        case ExprType_FuncCall: {
            FuncCallExpression* fc = (FuncCallExpression*)expr;
            return fc->call.function->returnType;
        } break;
    }

    // This is not supposed to ever hapen
    printf("getExpressedType could not determine type of expression, this is a bug!\n");
    numberOfErrors++;
    return (PlangType) {
        .structName = spFrom("err_no_type"),
        .numPointers = 0
    };
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
}


static PlangType* validateExpression(Expression* expr) {
    switch (expr->expressionType) {        
        case ExprType_Variable: {
            //ExpressionProxy* exp = (ExpressionProxy*)expr;
            //ValuePath* var = exp->node;
            //return validateValue(var);

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

            PlangStruct* stru = getStructByName(type->structName);
            Field* field = getField(stru, deref->name);
            if (!field) {
                error("Field \"%.*s\" does not exist on type \"%.*s\".",
                        field->name.length, field->name.start,
                        stru->name.length, stru->name.start);
                return null;
            }

            return &field->type;
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
            PlangType* size = null;
            if (alloc->sizeExpr) size = validateExpression(alloc->sizeExpr);

            if (size) {
                // TODO: is this a valid integer expression?
            }

            // TODO: Ugly hack here, find better memory storage for types. 
            // Or return PlangType by value perhaps?
            alloc->type.numPointers++;
            return &alloc->type;

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
            // TODO: determine type here
            FuncCallExpression* fc = (FuncCallExpression*)expr;
            validateFuncCall(&fc->call);
        } break;

        { // literals
            static PlangType int32type = (PlangType) { .structName = ((StrSpan) { .start = "int", .length = 3 }), .numPointers = 0 };
            static PlangType charP     = (PlangType) { .structName = ((StrSpan) { .start = "char", .length = 4 }), .numPointers = 1 };
            static PlangType voidP     = (PlangType) { .structName = ((StrSpan) { .start = "void", .length = 4 }), .numPointers = 1 };

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
                bool assigneeValid = validateValue(ass->assignee);
                PlangType* exprValid = validateExpression(ass->expr);
                if (assigneeValid && exprValid) {
                    PlangType asseeType = getExpressedTypeValuePath(ass->assignee);
                    PlangType exprType = getExpressedType(ass->expr);
                    if (!typeEquals(asseeType, exprType)) {
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
        PlangType assType;
        if (validateExpression(decl->assignmentOrNull)) assType = getExpressedType(decl->assignmentOrNull);
        else return; // if type could not be determined then we should not continue.

        if (decl->mustInferType) {
            decl->type = assType;
        } else {
            validateType(decl->type.structName);

            if (!typeEquals(decl->type, assType)) {
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

