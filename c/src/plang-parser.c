#include "plang-parser.h"
#include "plang-lexer.h"

// included for NULL
#include <stdlib.h>
#include <stdio.h>

static u32 token_index;


void printExpression(Expression* expr) {
    switch (expr->expressionType) {
        case ExprType_Variable:
        case ExprType_Number: 
            printf("%.*s", expr->value->length, expr->value->start); 
            break;

        case ExprType_Arithmetic: {
            for (u32 i = 0; i < expr->count; i++) {
                printExpression(expr->subExpressions[i]);
                printf(" / ");
            }
        }
    }
}


// returns false if the token can not be interpreted as a type
bool parseType(PlangType* type) {
    if (tokens[token_index].type == Tok_Word) {
        
        type->structName = tokens[token_index].value;
        token_index++;

        u8 np = 0;
        while (tokens[token_index++].type == Tok_Mul) np++;
        type->numPointers = np;
        token_index--;

        return true;
    }

    return false;
}

#define assertToken(tok) if (tokens[token_index++].type != tok) goto failCase;

#define nextToken(tok) (tokens[token_index++].type == tok)

bool parseVarDecl() {
    VarDecl decl;
    if (!parseType(&decl.type)) return false;

    // if (nextToken(Tok_Word))

    return true;
}

bool parseBlock() {
    u32 startingIndex = token_index;

    assertToken(Tok_OpenCurl)
    assertToken(Tok_CloseCurl)

    return true;
failCase:
    token_index = startingIndex;
    return false;
}

bool parseFunction() {
    u32 startingIndex = token_index;

    PlangFunction func;

    // type
    if (!parseType(&func.returnType)) goto failCase;

    // name
    if (tokens[token_index++].type != Tok_Word) goto failCase;    
    func.name = tokens[token_index].value;

    // args
    assertToken(Tok_OpenParen)
    assertToken(Tok_CloseParen)

    // body
    assertToken(Tok_OpenCurl)
    assertToken(Tok_CloseCurl)

    return true;
failCase:
    token_index = startingIndex;
    return false;
}


inline bool isOperator(TokenType type) {
    return type >= Tok_Plus && type <= Tok_Div;
}


Expression* parseLeafExpression() {
    Token* token = &tokens[token_index++];
    ExprType eType;

    switch (token->type) {
        case Tok_Word: {
            eType = ExprType_Variable;
        } break;
        case Tok_Number: {
            eType = ExprType_Number;
        } break;

        case Tok_OpenParen: {
            Expression* e = parseExpression();
            if (tokens[token_index++].type != Tok_CloseParen) 
                printf("Error: expected closing parenthese\n"); // TODO: proper error messages

            return e;
        } break;

        default: {
            token_index--;
            return NULL;
        }
    }

    Expression* expr = malloc(sizeof(Expression));
    expr->value = &token->value;
    expr->expressionType = eType;
    return expr;
}

Expression* parseExpression() {
    u32 startingIndex = token_index;

    Expression* leafExpr = parseLeafExpression();
    if (!leafExpr) goto failCase;

    u32 i = 0;
    Expression* temp[16]; // TODO: this temp array needs to go.
    temp[i++] = leafExpr;

    while (isOperator(tokens[token_index].type)) {

        // TODO: store operators in expression
        token_index++;
        temp[i++] = parseLeafExpression();
    }

    if (i == 1) return leafExpr;


    Expression* expr = malloc(sizeof(Expression));
    expr->expressionType = ExprType_Arithmetic;
    expr->count = i;
    expr->subExpressions = malloc(sizeof(Expression) * i);
    for (u32 j = 0; j < expr->count; j++) {
        expr->subExpressions[j] = temp[j];
    }

    // TODO: I'd like to know why this didnt work
    // while (i > 0) expr->subExpressions[--i] = temp[i];

    return expr;


failCase:
    token_index = startingIndex;
    return NULL;
}


void parse() {
    for (token_index = 0; token_index < tokens_length; token_index++) {

    }
}
