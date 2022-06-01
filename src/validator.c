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

inline bool typeEquals(Datatype a, Datatype b) {
    return a.typeIndex == b.typeIndex && a.numPointers == b.numPointers;
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
    u32 len = darrayLength(globalVariables);
    for (u32 i = 0; i < len; i++) {
        if (spanEqualsSpan(name, globalVariables[i].name)) return globalVariables[i].type;
    }

    return type_null;
}

static Datatype validateVariable(VariableExpression* var) {
    Datatype type = getDeclaredVariable(var->name);

    if (!type.typeIndex) {

        // TODO: temporary soulution until we get function pointers
        FuncDeclaration* decl = getFuncDecl(var->name);
        if (decl) return type_voidPointer;


        error("Variable \"%.*s\" is not declared.",
            var->name.length,
            var->name.start);

        return type_null;
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

// TODO: remove this when we add funcpointers
static StringBuilder sb_FuncPtr;

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
        if (varType.typeIndex) {
            // NOTE: we are not even asserting that it is a void pointer
            call->base.expressionType = ExprType_FuncPointerCall;

            // NOTE: lots of beautifull code here to make function pointers work.
            // It's very hacky, and probably wont be necessary when we get properly typed function pointers

            sbClear(&sb_FuncPtr);
            sbAppend(&sb_FuncPtr, "((void (*)(");

            u32 argLen = call->args ? darrayLength(call->args) : 0;
            for (u32 i = 0; i < argLen; i++) {
                Datatype t = validateExpression(call->args[0]);
                sbAppendSpan(&sb_FuncPtr, getType(t)->name);
                for (u32 j = 0; j < t.numPointers; j++) sbAppendChar(&sb_FuncPtr, '*');
                sbAppend(&sb_FuncPtr, ", ");
            }
            if (argLen) sb_FuncPtr.length -= 2;
            sbAppend(&sb_FuncPtr, "))");
            sbAppendSpan(&sb_FuncPtr, name);
            sbAppendChar(&sb_FuncPtr, ')');

            StrSpan funcPtr;
            funcPtr.length = sb_FuncPtr.length;
            funcPtr.start = malloc(sb_FuncPtr.length);
            sbCopyIntoBuffer(&sb_FuncPtr, funcPtr.start, funcPtr.length);

            call->functionName = funcPtr;

            return type_null; // temporary til we get function pointers
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
        if (passedArgType.typeIndex) {
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
            if (!type.typeIndex) return type_null;
            
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
            
            if (leftType.typeIndex && rightType.typeIndex) {
                // TODO: is a valid operator operands pair?
            } else return type_null;

            return leftType;
        } break;
        
        case ExprType_Alloc: {
            AllocExpression* alloc = (AllocExpression*)expr;
            validateType(getType(alloc->type)->name);

            // recurse on possibe size-subexpression
            if (alloc->sizeExpr) {
                Datatype size = validateExpression(alloc->sizeExpr);

                if (size.typeIndex) {
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
                if (getDeclaredVariable(decl->name).typeIndex) {
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
                    if (!assType.typeIndex) break; // if type could not be determined then we should not continue, act as if this statement does not exist 

                    if (decl->mustInferType) {
                        decl->type = assType;
                    } else {
                        validateType(getType(decl->type)->name);

                        if (!typeEquals(decl->type, assType)) {
                            error("Type missmatch in declaration.");
                        }
                    }

                } else {
                    validateType(getType(decl->type)->name);
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

                if (toType.typeIndex && fromType.typeIndex) {
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

                if (!type.typeIndex) break; 

                if (function->mustInferReturnType) {
                    function->decl.returnType = type;
                    function->mustInferReturnType = false;
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

static void validateFunction(PlangFunction* func) {
    function = func;

    if (!func->mustInferReturnType) {
        // TODO: factor all validateType calls out into own loop
        validateType(getType(func->decl.returnType)->name);
    }

    if (func->decl.arguments) {
        u32 len = darrayLength(func->decl.arguments);
        for (u32 i = 0; i < len; i++) validateType(getType(func->decl.arguments[i].type)->name);
    }

    currentScope = null;
    validateScope(&func->scope);

    if (func->mustInferReturnType) {
        func->decl.returnType = type_void;
        func->mustInferReturnType = false;
    }
}

static void validateStruct(PlangStruct* stru) {
    u32 fieldLen = darrayLength(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Field* field = &stru->fields[i];
        StrSpan fieldTypeName = getType(field->type)->name;
        validateType(fieldTypeName);

        if (field->type.numPointers == 0 && spanEqualsSpan(fieldTypeName, stru->name)) {
            errorLine(field->nodebase.lineNumber, "Struct \"%.*s\" self reference by value.",
                stru->name.length, stru->name.start);
        }
    }
}

static void validateGlobalVar(VarDecl* decl) {
    if (decl->assignmentOrNull) {
        Datatype assType = validateExpression(decl->assignmentOrNull);
        if (!assType.typeIndex) return; // if type could not be determined then we should not continue.

        if (decl->mustInferType) {
            decl->type = assType;
        } else {
            validateType(getType(decl->type)->name);

            if (!typeEquals(decl->type, assType)) {
                error("Type missmatch in global.");
            }
        }

    } else {
        validateType(getType(decl->type)->name);
    }
}

u32 validate() {

    type_void           = (Datatype) { .typeIndex = ensureTypeExistence(spFrom("void")), .numPointers = 0 };
    type_voidPointer    = (Datatype) { .typeIndex = ensureTypeExistence(spFrom("void")), .numPointers = 1 };
    type_int32          = (Datatype) { .typeIndex = ensureTypeExistence(spFrom("int")),  .numPointers = 0 };
    type_float32        = (Datatype) { .typeIndex = ensureTypeExistence(spFrom("float")), .numPointers = 0 };
    type_charPointer    = (Datatype) { .typeIndex = ensureTypeExistence(spFrom("char")), .numPointers = 1 };

    sb_FuncPtr = sbCreate();

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