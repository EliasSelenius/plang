
#include "parser.h"
#include "essh-string.h"
#include "darray.h"

static StringBuilder* sb;
static u32 tabing = 0;
static char* tabs[] = {
    "",
    "    ",
    "        ",
    "            "
};
inline void newline() {
    sbAppend(sb, "\n");
    sbAppend(sb, tabs[tabing]);
}

static void transpileType(PlangType type) {
    // TODO: transpile pointers
    sbAppendSpan(sb, type.structName);
}

static void transpileExpression(Expression* expr) {

    switch (expr->expressionType) {
        case ExprType_Arithmetic: {
            sbAppend(sb, "(");

            transpileExpression(expr->subExpressions[0]);
            for (u32 i = 1; i < expr->count; i++) {
                sbAppend(sb, " ");
                sbAppendSpan(sb, expr->operators[i - 1]);
                sbAppend(sb, " ");
                transpileExpression(expr->subExpressions[i]);
            }

            sbAppend(sb, ")");
        } break;

        case ExprType_Number:
        case ExprType_Variable: {
            sbAppendSpan(sb, expr->value);
        } break;

        case ExprType_Alloc: {
            AllocExpression* allocExpr = ((AllocExpression*)expr->node);
            sbAppend(sb, "malloc(sizeof(");
            transpileType(allocExpr->type);
            sbAppend(sb, "))");
        } break;
    }

}


static void transpileStatement(Statement* statement) {
    switch (statement->statementType) {
        case Statement_Declaration: {
            VarDecl* decl = (VarDecl*)statement->node;

            sbAppendSpan(sb, decl->type.structName);
            sbAppend(sb, " ");
            sbAppendSpan(sb, decl->name);

            if (decl->assignmentOrNull) {
                sbAppend(sb, " = ");
                transpileExpression(decl->assignmentOrNull);
            }

            sbAppend(sb, ";");
        } break;
        case Statement_Assignment: {

        } break;
        case Statement_If: {

        } break;
        case Statement_While: {

        } break;
    }
}

static void transpileFunction(PlangFunction* func) {
    sbAppendSpan(sb, func->returnType.structName);
    sbAppend(sb, " ");
    sbAppendSpan(sb, func->name);
    sbAppend(sb, "() {");
    tabing++;

    for (u32 i = 0; i < darrayLength(func->scope.statements); i++) {
        newline();
        transpileStatement(&func->scope.statements[i]);
    }

    tabing--;
    newline();

    sbAppend(sb, "}\n");
    
}

static void transpileStruct(PlangStruct* stru) {
    sbAppend(sb, "typedef struct ");
    sbAppendSpan(sb, stru->name);
    sbAppend(sb, " {");
    
    tabing++;

    for (u32 i = 0; i < darrayLength(stru->fields); i++) {
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



void transpile() {
    // TODO: use a higer initial capacity for the string builder
    StringBuilder builder = sbCreate();
    sb = &builder;

    for (u32 i = 0; i < darrayLength(structs); i++) {
        transpileStruct(&structs[i]);
    }

    for (u32 i = 0; i < darrayLength(functions); i++) {        
        PlangFunction* func = &functions[i];
        transpileFunction(func);
    }


    filewrite("Hello.g.c", sb->content);
    sbDestroy(sb);
}