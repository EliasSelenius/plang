
#include "parser.h"
#include "essh-string.h"
#include "darray.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h> // malloc

void transpileBlock(Codeblock* scope);
void filewrite(const char* filename, char* content);
void transpileExpression(Expression* expr);

static StringBuilder* sb;
static u32 tabing = 0;
inline void newline() {
    sbAppend(sb, "\n");
    u32 t = tabing;
    while (t--) sbAppend(sb, "    ");
}

StrSpan getTypeCname(Datatype type) {
    PlangType* ptype = getType(type);

    switch (ptype->kind) {
        case Typekind_Invalid: break;
        case Typekind_Primitive: {

            // TODO: this is obviously temporarly hard coded
            if (type.typeId == 9) return spFrom("signed long long");
            if (type.typeId == 10) return spFrom("unsigned long long");

        } break;
        case Typekind_Struct: break;
        case Typekind_Enum: break;
        case Typekind_Alias: break;
        case Typekind_FuncPtr: break;
    }

    return ptype->name;
}

static void transpileType(Datatype type) {
    sbAppendSpan(sb, getTypeCname(type));
    u32 np = type.numPointers;
    while (np-- > 0) {
        sbAppend(sb, "*");
    }
}

static char getCharFromU32(u32 num) {
    // TODO: do better than this...
    return '0' + num;
}

static void transpileFuncCall(FuncCall* call) {
    transpileExpression(call->funcExpr);
    if (call->overload) sbAppendChar(sb, getCharFromU32(call->overload));
    sbAppend(sb, "(");
    if (call->args) {
        transpileExpression(call->args[0]);

        u32 len = darrayLength(call->args);
        for (u32 i = 1; i < len; i++) {
            sbAppend(sb, ", ");
            transpileExpression(call->args[i]);
        }
    }
    sbAppend(sb, ")");
}

static void transpileExpression(Expression* expr) {

    switch (expr->expressionType) {

        { // Binary & Unary Expressions

            case ExprType_Unary_PostIncrement: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                transpileExpression(unary->expr);
                sbAppend(sb, "++");
            } break;
            case ExprType_Unary_PostDecrement: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                transpileExpression(unary->expr);
                sbAppend(sb, "--");
            } break;


            char* operator = null;

            case ExprType_Unary_PreIncrement: operator = "++"; goto unary;
            case ExprType_Unary_PreDecrement: operator = "--"; goto unary;
            case ExprType_Unary_Not:          operator = "!"; goto unary;
            case ExprType_Unary_AddressOf:    operator = "&"; goto unary;
            case ExprType_Unary_ValueOf:      operator = "*"; goto unary;
            case ExprType_Unary_Negate:       operator = "-"; goto unary;
            case ExprType_Unary_BitwiseNot:   operator = "~"; goto unary;

            case ExprType_Bitwise_And: operator = " & "; goto binary;
            case ExprType_Bitwise_Or: operator = " | "; goto binary;
            case ExprType_Bitwise_Xor: operator = " ^ "; goto binary;
            case ExprType_Bitwise_Lshift: operator = " << "; goto binary;
            case ExprType_Bitwise_Rshift: operator = " >> "; goto binary;

            case ExprType_Plus:  operator = " + "; goto binary;
            case ExprType_Minus: operator = " - "; goto binary;
            case ExprType_Mul:   operator = " * "; goto binary;
            case ExprType_Div:   operator = " / "; goto binary;
            case ExprType_Mod:   operator = " % "; goto binary;

            case ExprType_Less:          operator = " < ";  goto binary;
            case ExprType_Greater:       operator = " > ";  goto binary;
            case ExprType_LessEquals:    operator = " <= "; goto binary;
            case ExprType_GreaterEquals: operator = " >= "; goto binary;
            case ExprType_Equals:        operator = " == "; goto binary;
            case ExprType_NotEquals:     operator = " != "; goto binary;
            case ExprType_BooleanAnd:    operator = " && "; goto binary;
            case ExprType_BooleanOr:     operator = " || "; goto binary;

            unary: {
                UnaryExpression* unary = (UnaryExpression*)expr;
                sbAppend(sb, operator);
                transpileExpression(unary->expr);
            } break;

            binary: {
                BinaryExpression* bop = (BinaryExpression*)expr;
                sbAppend(sb, "(");
                transpileExpression(bop->left);
                sbAppend(sb, operator);
                transpileExpression(bop->right);
                sbAppend(sb, ")");
            } break;
        }

        case ExprType_Deref: {
            DerefOperator* deref = (DerefOperator*)expr;
            transpileExpression(deref->expr);
            sbAppend(sb, deref->derefOp);
            sbAppendSpan(sb, deref->name);
        } break;

        case ExprType_Indexing: {
            IndexingExpression* ind = (IndexingExpression*)expr;
            transpileExpression(ind->indexed);
            sbAppend(sb, "[");
            transpileExpression(ind->index);
            sbAppend(sb, "]");
        } break;

        case ExprType_Literal_Null: {
            sbAppend(sb, "0");
        } break;


        case ExprType_Literal_Bool:
        case ExprType_Literal_String:
        case ExprType_Literal_Char:

        case ExprType_Literal_Integer:
        case ExprType_Literal_Uint:
        case ExprType_Literal_Long:
        case ExprType_Literal_ULong:
        case ExprType_Literal_Decimal:
        case ExprType_Literal_Float:
        case ExprType_Literal_Double:
        {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendSpan(sb, lit->value);
        } break;

        case ExprType_Variable: {
            VariableExpression* var = (VariableExpression*)expr;
            sbAppendSpan(sb, var->name);
        } break;

        case ExprType_Constant: {
            VariableExpression* var = (VariableExpression*)expr;
            transpileExpression(var->constExpr);
        } break;

        case ExprType_Alloc: {
            AllocExpression* allocExpr = (AllocExpression*)expr;
            sbAppend(sb, "malloc(sizeof(");
            transpileType(allocExpr->type);
            sbAppend(sb, ")");
            if (allocExpr->sizeExpr) {
                sbAppend(sb, " * ");
                transpileExpression(allocExpr->sizeExpr);
            }
            sbAppend(sb, ")");
        } break;

        case ExprType_Sizeof: {
            SizeofExpression* sof = (SizeofExpression*)expr;
            sbAppend(sb, "sizeof(");
            transpileType(sof->type);
            sbAppend(sb, ")");
        } break;

        case ExprType_Cast: {
            CastExpression* cast = (CastExpression*)expr;
            sbAppend(sb, "(");
            transpileType(cast->castToType);
            sbAppend(sb, ")");
            transpileExpression(cast->expr);
        } break;

        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            transpileExpression(ter->condition);
            sbAppend(sb, " ? ");
            transpileExpression(ter->thenExpr);
            sbAppend(sb, " : ");
            transpileExpression(ter->elseExpr);
        } break;

        case ExprType_FuncPointerCall:
        case ExprType_FuncCall: {
            FuncCall* fc = (FuncCall*)expr;
            transpileFuncCall(fc);
        } break;

        case ExprType_Parenthesized: {
            ParenthesizedExpression* p = (ParenthesizedExpression*)expr;
            if (isBinaryExpression(p->innerExpr)) {
                transpileExpression(p->innerExpr);
            } else {
                sbAppend(sb, "(");
                transpileExpression(p->innerExpr);
                sbAppend(sb, ")");
            }
        } break;
    }

}

static void transpileCondition(Expression* cond) {
    if (isBinaryExpression(cond)) {
        transpileExpression(cond);
    } else {
        sbAppend(sb, "(");
        transpileExpression(cond);
        sbAppend(sb, ")");
    }
}

static void transpileIfStatement(IfStatement* ifst) {
    if (ifst->condition) {
        sbAppend(sb, "if ");
        transpileCondition(ifst->condition);
        sbAppend(sb, " ");
    }

    transpileBlock(&ifst->scope);

    if (ifst->next) {
        sbAppend(sb, " else ");
        transpileIfStatement(ifst->next);
    }
}

static void transpileVarDecl(VarDecl* decl) {
    if (decl->base.statementType == Statement_FixedArray_Declaration) {
        decl->type.numPointers--;
        transpileType(decl->type);
        decl->type.numPointers++;
        sbAppend(sb, " ");
        sbAppendSpan(sb, decl->name);

        sbAppend(sb, "[");
        transpileExpression(decl->assignmentOrNull);
        sbAppend(sb, "]");
    } else {
        transpileType(decl->type);
        sbAppend(sb, " ");
        sbAppendSpan(sb, decl->name);

        if (decl->assignmentOrNull) {
            sbAppend(sb, " = ");
            transpileExpression(decl->assignmentOrNull);
        }
    }

    sbAppend(sb, ";");
}

static void transpileStatement(Statement* statement) {
    switch (statement->statementType) {
        case Statement_FixedArray_Declaration:
        case Statement_Declaration: {
            VarDecl* decl = (VarDecl*)statement;
            transpileVarDecl(decl);
        } break;
        case Statement_Assignment: {
            Assignement* ass = (Assignement*)statement;
            transpileExpression(ass->assigneeExpr);

            switch (ass->assignmentOper) {
                case Tok_Assign: sbAppend(sb, " = "); break;
                case Tok_PlusAssign: sbAppend(sb, " += "); break;
                case Tok_MinusAssign: sbAppend(sb, " -= "); break;
                case Tok_MulAssign: sbAppend(sb, " *= "); break;
                case Tok_DivAssign: sbAppend(sb, " /= "); break;
                default: break;
            }

            transpileExpression(ass->expr);
            sbAppend(sb, ";");        
        } break;

        case Statement_Scope: {
            transpileBlock(&((Scope*)statement)->codeblock);
        } break;
        case Statement_If: {
            transpileIfStatement((IfStatement*)statement);
        } break;
        case Statement_While: {
            WhileStatement* sta = (WhileStatement*)statement;
            sbAppend(sb, "while ");
            transpileCondition(sta->condition);
            sbAppend(sb, " ");
            transpileBlock(&sta->scope);
        } break;

        case Statement_Break: sbAppend(sb, "break;"); break;
        case Statement_Continue: sbAppend(sb, "continue;"); break;

        case Statement_Return: {
            ReturnStatement* retSta = (ReturnStatement*)statement;
            if (retSta->returnExpr) {
                sbAppend(sb, "return ");
                transpileExpression(retSta->returnExpr);
                sbAppend(sb, ";");
            } else {
                sbAppend(sb, "return;");
            }
            
        } break;

        case Statement_Expression: {
            StatementExpression* staExpr = (StatementExpression*)statement;
            transpileExpression(staExpr->expr);
            sbAppend(sb, ";");
        } break;
    }
}

static void transpileBlock(Codeblock* scope) {
    sbAppend(sb, "{");
    tabing++;

    u32 len = darrayLength(scope->statements);
    for (u32 i = 0; i < len; i++) {
        newline();
        transpileStatement(scope->statements[i]);
    }

    tabing--;
    newline();

    sbAppend(sb, "}");
}

static void transpileFunctionSignature(FuncDeclaration* func, u32 overload) {
    transpileType(func->returnType);
    sbAppend(sb, " ");
    sbAppendSpan(sb, func->name);
    if (overload) sbAppendChar(sb, getCharFromU32(overload));
    sbAppend(sb, "(");
    if (func->arguments) {

        FuncArg arg = func->arguments[0];
        transpileType(arg.type);
        sbAppend(sb, " ");
        sbAppendSpan(sb, arg.name);

        u32 len = darrayLength(func->arguments);
        for (u32 i = 1; i < len; i++) {
            sbAppend(sb, ", ");

            arg = func->arguments[i];
            transpileType(arg.type);
            sbAppend(sb, " ");
            sbAppendSpan(sb, arg.name);
        }
    }
    sbAppend(sb, ")");
}

static void transpileFunction(PlangFunction* func) {
    transpileFunctionSignature(&func->decl, func->overload);
    sbAppend(sb, " ");
    transpileBlock(&func->scope);
    newline();
}

static void transpileStruct(PlangStruct* stru) {
    sbAppend(sb, "typedef struct ");
    sbAppendSpan(sb, stru->name);
    sbAppend(sb, " {");

    tabing++;

    u32 len = darrayLength(stru->fields);
    for (u32 i = 0; i < len; i++) {
        newline();
        transpileType(stru->fields[i].type);
        sbAppend(sb, " ");
        sbAppendSpan(sb, stru->fields[i].name);
        sbAppend(sb, ";");
    }

    tabing--;
    newline();

    sbAppend(sb, "} ");
    sbAppendSpan(sb, stru->name);
    sbAppend(sb, ";\n");
}

static u32 countStructDependencies(PlangStruct* stru) {
    u32 deps = 0;
    u32 fieldsLen = darrayLength(stru->fields);
    for (u32 f = 0; f < fieldsLen; f++) {
        Datatype datatype = stru->fields[f].type;
        if (datatype.numPointers) continue;
        PlangType* type = getActualType(datatype);
        if (type->kind != Typekind_Struct) continue;

        deps++;
        deps += countStructDependencies(type->type_struct);
    }

    return deps;
}

void transpile() {
    // TODO: use a higer initial capacity for the string builder
    StringBuilder builder = sbCreate();
    sb = &builder;

    // sbAppend(sb, "#include <stdlib.h>\n"); // malloc
    // sbAppend(sb, "#include <stdio.h>\n"); // printf
    sbAppend(sb, "#define true 1\n#define false 0\n");

    sbAppend(sb, "\n// types\n");
    sbAppend(sb, "typedef unsigned int uint;\n");
    sbAppend(sb, "typedef unsigned char byte;\n");
    sbAppend(sb, "typedef signed char sbyte;\n");
    sbAppend(sb, "typedef unsigned short ushort;\n");

    u32 typesLen = darrayLength(g_Unit->types);
    for (u32 i = 0; i < typesLen; i++) {
        PlangType* type = &g_Unit->types[i];
        switch (type->kind) {

            case Typekind_Invalid: break;
            case Typekind_Primitive: break;

            case Typekind_Struct: {
                sbAppend(sb, "typedef struct ");
                sbAppendSpan(sb, type->type_struct->name);
                sbAppend(sb, " ");
                sbAppendSpan(sb, type->type_struct->name);
                sbAppend(sb, ";\n");
            } break;

            case Typekind_Enum: break;

            case Typekind_Alias: {
                sbAppend(sb, "typedef ");
                if (type->type_aliasedType.typeId) {
                    transpileType(type->type_aliasedType);
                } else {
                    sbAppend(sb, "struct ");
                    sbAppendSpan(sb, type->name);
                }
                sbAppend(sb, " ");
                sbAppendSpan(sb, type->name);
                sbAppend(sb, ";\n");
            } break;

            case Typekind_FuncPtr: {
                FuncPtr* funcPtr = getFuncPtr(type->type_funcPtr);
                sbAppend(sb, "typedef ");
                transpileType(funcPtr->returnType);
                sbAppend(sb, " ");
                sbAppendSpan(sb, type->name);
                sbAppend(sb, "(");
                if (funcPtr->argCount) {
                    transpileType(funcPtr->argTypes[0]);
                    for (u32 i = 1; i < funcPtr->argCount; i++) {
                        sbAppend(sb, ", ");
                        transpileType(funcPtr->argTypes[i]);
                    }
                }
                sbAppend(sb, ");\n");

            } break;
        }
    }

    { // structs
        sbAppend(sb, "\n// Structs\n");
        u32 structsLen = darrayLength(g_Unit->structs);
        u32* deps = malloc(sizeof(u32) * structsLen);

        for (u32 i = 0; i < structsLen; i++) {
            PlangStruct* stru = &g_Unit->structs[i];

            deps[i] = countStructDependencies(stru);
        }

        u32 dep = 0;
        u32 transpiled = 0;
        while (transpiled < structsLen) {
            for (u32 i = 0; i < structsLen; i++) {
                if (deps[i] == dep) {
                    PlangStruct* stru = &g_Unit->structs[i];
                    char buffer[16];
                    sprintf_s(buffer, 16, "// deps: %d\n", deps[i]);
                    sbAppend(sb, buffer);
                    transpileStruct(stru);
                    transpiled++;
                }
            }
            dep++;
        }

        free(deps);
    }

    sbAppend(sb, "\n// Forward declarations\n");
    u32 functionsLen = darrayLength(g_Unit->functions);
    for (u32 i = 0; i < functionsLen; i++) {
        PlangFunction* func = &g_Unit->functions[i];

        transpileFunctionSignature(&func->decl, func->overload);
        sbAppend(sb, ";\n");
    }

    u32 declsLength = darrayLength(g_Unit->functionDeclarations);
    for (u32 i = 0; i < declsLength; i++) {
        transpileFunctionSignature(&g_Unit->functionDeclarations[i], 0);
        sbAppend(sb, ";\n");
    }


    sbAppend(sb, "\n// Globals\n");
    u32 globLen = darrayLength(g_Unit->globalVariables);
    for (u32 i = 0; i < globLen; i++) {
        transpileVarDecl(&g_Unit->globalVariables[i]);
        sbAppend(sb, "\n");
    }

    sbAppend(sb, "\n// Implementations\n");

    for (u32 i = 0; i < functionsLen; i++) {
        PlangFunction* func = &g_Unit->functions[i];
        transpileFunction(func);
    }


    FILE* file;
    if ( !fopen_s(&file, "output.g.c", "w") ) {
        fprintf(file, "%s", sb->content);
        fclose(file);
    } else {
        printf("Could not write to output.g.c\n");
    }


    sbDestroy(sb);
}