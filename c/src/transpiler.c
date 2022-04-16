
#include "parser.h"
#include "essh-string.h"
#include "darray.h"

#include <stdio.h>

void transpileBlock(Codeblock* scope);
void filewrite(const char* filename, char* content);
void transpileValuePath(ValuePath* value);
void transpileExpression(Expression* expr);

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
    sbAppendSpan(sb, type.structName);
    u32 np = type.numPointers;
    while (np-- > 0) {
        sbAppend(sb, "*");
    }
}

static void transpileFuncCall(FuncCall* func) {
    sbAppendSpan(sb, func->function->name);
    sbAppend(sb, "(");
    if (func->args) {
        transpileExpression(func->args[0]);
        
        u32 len = darrayLength(func->args);
        for (u32 i = 1; i < len; i++) {
            sbAppend(sb, ", ");
            transpileExpression(func->args[i]);
        }
    }
    sbAppend(sb, ")");
}

static void transpileExpression(Expression* expr) {

    switch (expr->expressionType) {
        case ExprType_Arithmetic: {
            sbAppend(sb, "(");

            ArithmeticExpression* arith = (ArithmeticExpression*)expr;

            transpileExpression(arith->subExpressions[0]);
            for (u32 i = 1; i < arith->count; i++) {
                sbAppend(sb, " ");
                sbAppendSpan(sb, arith->operators[i - 1]);
                sbAppend(sb, " ");
                transpileExpression(arith->subExpressions[i]);
            }

            sbAppend(sb, ")");
        } break;

        case ExprType_Literal_Null: {
            sbAppend(sb, "0");
        } break;

        case ExprType_Literal_Bool:
        case ExprType_Literal_String:
        case ExprType_Literal_Number: {
            LiteralExpression* lit = (LiteralExpression*)expr;
            sbAppendSpan(sb, lit->value);
        } break;

        case ExprType_Variable: {
            transpileValuePath(((ExpressionProxy*)expr)->node);
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

        case ExprType_Ternary: {
            TernaryExpression* ter = (TernaryExpression*)expr;
            transpileExpression(ter->condition);
            sbAppend(sb, " ? ");
            transpileExpression(ter->thenExpr);
            sbAppend(sb, " : ");
            transpileExpression(ter->elseExpr);
        } break;

        case ExprType_FuncCall: {
            FuncCallExpression* fc = (FuncCallExpression*)expr;
            transpileFuncCall(&fc->call);
        } break;
    }

}

static void transpileValuePath(ValuePath* value) {
    sbAppendSpan(sb, value->name);
    
    u32 numPointers = value->type->numPointers;

    if (value->index) {
        numPointers--;

        sbAppend(sb, "[");
        transpileExpression(value->index);
        sbAppend(sb, "]");
    }


    if (value->next) {
        if (numPointers == 0) sbAppend(sb, ".");
        else if (numPointers == 1) sbAppend(sb, "->");
        else printf("transpileValuePath could not transpile valuepath, this is a bug! the validator should have made sure this never happened\n");

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
            VarDecl* decl = (VarDecl*)statement;

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
            Assignement* ass = (Assignement*)statement;
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

        case Statement_Scope: {
            transpileBlock(&((Scope*)statement)->codeblock);
        } break;
        case Statement_If: {
            transpileIfStatement((IfStatement*)statement);
        } break;
        case Statement_While: {
            WhileStatement* sta = (WhileStatement*)statement;
            sbAppend(sb, "while (");
            transpileExpression(sta->condition);
            sbAppend(sb, ") ");
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

        case Statement_FuncCall: {
            FuncCallStatement* funcCallSta = (FuncCallStatement*)statement;
            transpileFuncCall(&funcCallSta->call);
            sbAppend(sb, ";");
        } break;
    }
}

static void transpileBlock(Codeblock* scope) {
    sbAppend(sb, "{");
    tabing++;

    for (u32 i = 0; i < darrayLength(scope->statements); i++) {
        newline();
        transpileStatement(scope->statements[i]);
    }

    tabing--;
    newline();

    sbAppend(sb, "}");
}

static void transpileFunctionSignature(FuncDeclaration* func) {
    transpileType(func->returnType);
    sbAppend(sb, " ");
    sbAppendSpan(sb, func->name);
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
    if (func->mustInferReturnType) return; // TODO: remove this

    transpileFunctionSignature(&func->decl);
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

    sbAppend(sb, "#include <stdlib.h>\n"); // malloc
    sbAppend(sb, "#include <stdio.h>\n"); // printf
    sbAppend(sb, "#define true 1\n#define false 0\n");

    sbAppend(sb, "// Structs\n");
    u32 structsLen = darrayLength(structs);
    for (u32 i = 0; i < structsLen; i++) {
        transpileStruct(&structs[i]);
    }

    sbAppend(sb, "\n// Forward declarations\n");
    u32 functionsLen = darrayLength(functions);
    for (u32 i = 0; i < functionsLen; i++) {
        PlangFunction* func = &functions[i];

        if (func->mustInferReturnType) continue; // TODO: remove this

        transpileFunctionSignature(&func->decl);
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