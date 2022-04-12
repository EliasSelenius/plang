
#include "parser.h"
#include "essh-string.h"
#include "darray.h"

void transpileBlock(Codeblock* scope);
void filewrite(const char* filename, char* content);
void transpileValuePath(ValuePath* value);


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
    u32 np = type.numPointers;
    while (np-- > 0) {
        sbAppend(sb, "*");
    }
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

        case ExprType_Null: {
            sbAppend(sb, "0");
        } break;

        case ExprType_Bool:
        case ExprType_String:
        case ExprType_Number: {
            sbAppendSpan(sb, expr->value);
        } break;

        case ExprType_Variable: {
            transpileValuePath(expr->node);
        } break;

        case ExprType_Alloc: {
            AllocExpression* allocExpr = ((AllocExpression*)expr->node);
            sbAppend(sb, "malloc(sizeof(");
            transpileType(allocExpr->type);
            sbAppend(sb, ")");

            if (allocExpr->sizeExpr) {
                sbAppend(sb, " * ");
                transpileExpression(allocExpr->sizeExpr);
            }

            sbAppend(sb, ")");

        } break;

        case ExprType_Ternary: {
            TernaryExpression* ter = expr->node;
            transpileExpression(ter->condition);
            sbAppend(sb, " ? ");
            transpileExpression(ter->thenExpr);
            sbAppend(sb, " : ");
            transpileExpression(ter->elseExpr);
        } break;
    }

}

static void transpileValuePath(ValuePath* value) {
    sbAppendSpan(sb, value->name);
    if (value->index) {
        sbAppend(sb, "[");
        transpileExpression(value->index);
        sbAppend(sb, "]");
    }

    if (value->next) {
        sbAppend(sb, ".");
        transpileValuePath(value->next);
    }
}

static void transpileIfStatement(IfStatement* ifst) {
    if (ifst->condition) {
        sbAppend(sb, "if (");
        transpileExpression(ifst->condition);
        sbAppend(sb, ") ");
    }

    transpileBlock(&ifst->scope);

    if (ifst->next) {
        sbAppend(sb, " else ");
        transpileIfStatement(ifst->next);
    }
}

static void transpileStatement(Statement* statement) {
    switch (statement->statementType) {
        case Statement_Declaration: {
            VarDecl* decl = (VarDecl*)statement->node;

            transpileType(decl->type);
            sbAppend(sb, " ");
            sbAppendSpan(sb, decl->name);

            if (decl->assignmentOrNull) {
                sbAppend(sb, " = ");
                transpileExpression(decl->assignmentOrNull);
            }

            sbAppend(sb, ";");
        } break;
        case Statement_Assignment: {
            Assignement* ass = statement->node;
            transpileValuePath(ass->assignee);

            switch (ass->assignmentOper) {
                case Tok_Assign: sbAppend(sb, " = "); break;
                case Tok_PlusEquals: sbAppend(sb, " += "); break;
                case Tok_MinusEquals: sbAppend(sb, " -= "); break;
                case Tok_MulEquals: sbAppend(sb, " *= "); break;
                case Tok_DivEquals: sbAppend(sb, " /= "); break;
                default: break;
            }

            transpileExpression(ass->expr);
            sbAppend(sb, ";");            
        } break;
        case Statement_If: {
            transpileIfStatement(statement->node);
        } break;
        case Statement_While: {
            WhileStatement* sta = (WhileStatement*)statement->node;
            sbAppend(sb, "while (");
            transpileExpression(sta->condition);
            sbAppend(sb, ") ");
            transpileBlock(&sta->scope);
        } break;

        case Statement_Break: sbAppend(sb, "break;"); break;
        case Statement_Continue: sbAppend(sb, "continue;"); break;

        case Statement_Return: {
            if (statement->node) {
                sbAppend(sb, "return ");
                transpileExpression(statement->node);
                sbAppend(sb, ";");
            } else {
                sbAppend(sb, "return;");
            }
            
        } break;

        case Statement_FuncCall: {
            FuncCall* func = statement->node;
            transpileValuePath(func->valuePath);
            sbAppend(sb, "(");
            if (func->args) {
                transpileExpression(func->args[0]);
                
                u32 len = darrayLength(func->args);
                for (u32 i = 1; i < len; i++) {
                    sbAppend(sb, ", ");
                    transpileExpression(func->args[i]);
                }
            }
            sbAppend(sb, ");");
        } break;
    }
}

static void transpileBlock(Codeblock* scope) {
    sbAppend(sb, "{");
    tabing++;

    for (u32 i = 0; i < darrayLength(scope->statements); i++) {
        newline();
        transpileStatement(&scope->statements[i]);
    }

    tabing--;
    newline();

    sbAppend(sb, "}");
}

static void transpileFunctionSignature(PlangFunction* func) {
    transpileType(func->returnType);
    sbAppend(sb, " ");
    sbAppendSpan(sb, func->name);
    sbAppend(sb, "()");
}

static void transpileFunction(PlangFunction* func) {
    transpileFunctionSignature(func);
    sbAppend(sb, " ");
    transpileBlock(&func->scope);
    newline();
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

    sbAppend(sb, "// Structs\n");
    u32 structsLen = darrayLength(structs);
    for (u32 i = 0; i < structsLen; i++) {
        transpileStruct(&structs[i]);
    }

    sbAppend(sb, "\n// Forward declarations\n");
    u32 functionsLen = darrayLength(functions);
    for (u32 i = 0; i < functionsLen; i++) {
        transpileFunctionSignature(&functions[i]);
        sbAppend(sb, ";\n");
    }
    
    sbAppend(sb, "\n// Implementations\n");

    for (u32 i = 0; i < functionsLen; i++) {     
        PlangFunction* func = &functions[i];
        transpileFunction(func);
    }


    filewrite("output.g.c", sb->content);
    sbDestroy(sb);
}