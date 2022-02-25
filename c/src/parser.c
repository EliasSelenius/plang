#include "parser.h"
#include "lexer.h"
#include "darray.h"

// included for NULL
#include <stdlib.h>
#include <stdio.h>

void expectBlock(Codeblock* scope);
Expression* parseExpression();
Expression* expectExpression();

static u32 token_index;

PlangFunction* functions = NULL;
PlangStruct* structs = NULL;




/*
inline void printError(char* format, char* msg) {
    printf("Error Ln%d. ", tokens[token_index].line);
    printf(format, msg);
}*/

// asserts the existence of a semicolon
inline void semicolon() {
    if (tokens[token_index].type != Tok_Semicolon) {        
        printf("Error Ln%d. Expected semicolon, but got \"%.*s\" instead.\n", 
            tokens[token_index].line,
            tokens[token_index].value.length,
            tokens[token_index].value.start);
        return;
    }
    token_index++;
}

static StrSpan identifier() {
    Token* token = &tokens[token_index];
    if (token->type != Tok_Word) {
        printf("Error Ln%d. \"%.*s\" is not a valid identifier.\n",
            token->line,
            token->value.length,
            token->value.start);
    }
    token_index++;
    return token->value;
}

static void expect(TokenType type) {
    if (tokens[token_index].type != type) {
        // TODO: get TokenType as string
        printf("Error Ln%d. Expected token type %d, but got \"%.*s\" instead.\n",
            tokens[token_index].line,
            type,
            tokens[token_index].value.length,
            tokens[token_index].value.start);
        return;
    }
    token_index++;
}


// returns false if the token can not be interpreted as a type
static bool parseType(PlangType* type) {
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

static PlangType expectType() {
    PlangType res = {0};
    if (!parseType(&res)) {
        printf("Error Ln%d. Expected type, but got \"%.*s\" instead.\n",
            tokens[token_index].line,
            tokens[token_index].value.length,
            tokens[token_index].value.start);
    }
    return res;
}


inline bool isOperator(TokenType type) {
    return type >= Tok_Plus && type <= Tok_Div;
}


static Expression* parseLeafExpression() {
    Token* token = &tokens[token_index++];
    ExprType eType;

    switch (token->type) {
        case Tok_Word: {
            eType = ExprType_Variable;
        } break;
        case Tok_Number: {
            eType = ExprType_Number;
        } break;
        case Tok_String: {
            eType = ExprType_String;
        } break;

        case Tok_Keyword_False:
        case Tok_Keyword_True: {
            eType = ExprType_Bool;
        } break;

        case Tok_Keyword_Null: {
            eType = ExprType_Null;
        } break;

        case Tok_OpenParen: {
            Expression* e = parseExpression();
            expect(Tok_CloseParen);
            return e;
        } break;

        case Tok_Keyword_Alloc: {
            AllocExpression* allocExpr = malloc(sizeof(AllocExpression));
            allocExpr->type = expectType();
            allocExpr->sizeExpr = NULL;
            
            if (tokens[token_index].type == Tok_OpenSquare) {
                token_index++;
                allocExpr->sizeExpr = expectExpression();
                expect(Tok_CloseSquare);
            }

            Expression* expr = malloc(sizeof(Expression));
            expr->node = allocExpr;
            expr->expressionType = ExprType_Alloc;


            return expr;        
        } break;

        default: {
            token_index--;
            return NULL;
        }
    }

    Expression* expr = malloc(sizeof(Expression));
    expr->value = token->value;
    expr->expressionType = eType;
    return expr;
}

// returns NULL if it fails to parse an expression
static Expression* parseExpression() {

    Expression* leafExpr = parseLeafExpression();
    if (!leafExpr) return NULL;

    u32 i = 0;
    
    Expression* temp[16]; // TODO: these arrays needs to go.
    temp[i++] = leafExpr;

    StrSpan ops[15];

    while (isOperator(tokens[token_index].type)) {
        
        ops[i - 1] = tokens[token_index].value;

        token_index++;
        temp[i++] = parseLeafExpression();
    }

    if (i == 1) return leafExpr;


    Expression* expr = malloc(sizeof(Expression));
    expr->expressionType = ExprType_Arithmetic;
    expr->count = i;
    expr->operators = malloc(sizeof(StrSpan) * (i - 1));
    expr->subExpressions = malloc(sizeof(Expression) * i);

    for (u32 j = 0; j < expr->count; j++)
        expr->subExpressions[j] = temp[j];

    for (u32 j = 0; j < expr->count - 1; j++)
        expr->operators[j] = ops[j];

    // TODO: I'd like to know why this didnt work
    // while (i > 0) expr->subExpressions[--i] = temp[i];

    return expr;
}

static Expression* expectExpression() {
    Expression* res = parseExpression();
    if (res == NULL) {
        printf("Error Ln%d. Expected expression, but got \"%.*s\" instead.\n",
            tokens[token_index].line,
            tokens[token_index].value.length,
            tokens[token_index].value.start);
    }
    return res;
}


#define assertToken(tok) if (tokens[token_index++].type != tok) goto failCase;

#define nextToken(tok) (tokens[token_index++].type == tok)

static VarDecl* parseVarDecl() {
    u32 startingIndex = token_index;
    bool useTypeInference = false;
    PlangType type;
    if (!parseType(&type)) {
        if (tokens[token_index].type == Tok_Keyword_Let) {
            useTypeInference = true;
            token_index++;
        } else {
            goto failCase;
        }
    }

    Token nameToken = tokens[token_index];
    if (!nextToken(Tok_Word)) goto failCase;

    Expression* assign = NULL;
    if (tokens[token_index].type == Tok_Assign) {
        token_index++;
        assign = expectExpression();
    }

    VarDecl* res = malloc(sizeof(VarDecl));
    res->mustInferType = useTypeInference;
    res->type = type;
    res->name = nameToken.value;
    res->assignmentOrNull = assign;
    return res;
failCase:
    token_index = startingIndex;
    return NULL;
}


static bool parse_If_While_Statement(Statement* statement) {

    if (tokens[token_index].type == Tok_Keyword_If) statement->statementType = Statement_If;
    else if (tokens[token_index].type == Tok_Keyword_While) statement->statementType = Statement_While;
    else return false;
    
    token_index++;

    If_While_Statement* res = malloc(sizeof(If_While_Statement));

    expect(Tok_OpenParen);
    res->condition = expectExpression();
    expect(Tok_CloseParen);

    expectBlock(&res->scope);

    statement->node = res;

    return true;
}

static bool parseStatement(Statement* statement) {

    if ( (statement->node = parseVarDecl()) ) {
        statement->statementType = Statement_Declaration;
        semicolon();
    } 
    else if ( parse_If_While_Statement(statement) );
    else if (tokens[token_index].type == Tok_Keyword_Continue) {
        statement->statementType = Statement_Continue;
        statement->node = NULL;
        token_index++;
        semicolon();
    }
    else if (tokens[token_index].type == Tok_Keyword_Break) {
        statement->statementType = Statement_Break;
        statement->node = NULL;
        token_index++;
        semicolon();
    }
    else if (tokens[token_index].type == Tok_Keyword_Return) {
        token_index++;
        statement->statementType = Statement_Return;
        statement->node = parseExpression();
        semicolon();
    }
    else {
        return false;
    }


    return true;
}

static bool parseBlock(Codeblock* scope) {
    u32 startingIndex = token_index;

    assertToken(Tok_OpenCurl)


    scope->statements = darrayCreate(Statement);
    
    
    Statement statement;
    while (parseStatement(&statement)) {

        darrayAdd(scope->statements, statement);
    }
    

    assertToken(Tok_CloseCurl)

    return true;
failCase:
    token_index = startingIndex;
    return false;
}

static void expectBlock(Codeblock* scope) {
    if (!parseBlock(scope)) {
        printf("Error Ln%d. Expected block, but got \"%.*s\" instead.\n",
            tokens[token_index].line,
            tokens[token_index].value.length,
            tokens[token_index].value.start);
    }
}

static bool parseFunction() {
    u32 startingIndex = token_index;


    PlangFunction func;

    // type
    if (!parseType(&func.returnType)) goto failCase;

    // name
    Token* nameToken = &tokens[token_index++];
    if (nameToken->type != Tok_Word) goto failCase;    
    func.name = nameToken->value;

    // args
    assertToken(Tok_OpenParen)
    // TODO: function arguments
    assertToken(Tok_CloseParen)

    // body
    if (!parseBlock(&func.scope)) goto failCase;

    darrayAdd(functions, func);
    return true;
failCase:
    token_index = startingIndex;
    return false;
}

static bool parseStruct() {

    if (tokens[token_index].type != Tok_Keyword_Struct) return false;
    token_index++;

    PlangStruct stru;

    stru.name = identifier();

    if (tokens[token_index++].type != Tok_OpenCurl) {
        printf("Expected open curlybracket.\n");
    }

    stru.fields = darrayCreate(Field);

    do {
        Field field;
        if (!parseType(&field.type)) {
            printf("Expected type\n");
        }

        field.name = identifier();

        semicolon();

        darrayAdd(stru.fields, field);

    } while (tokens[token_index].type != Tok_CloseCurl);
    token_index++;

    darrayAdd(structs, stru);

    return true;
}


void parse() {

    // TODO: PlangFile
    functions = darrayCreate(PlangFunction);
    structs = darrayCreate(PlangStruct);

    token_index = 0;
    while (token_index < tokens_length) {
        if (parseFunction()) continue;
        if (parseStruct()) continue;

        printf("Error while parsing. At line %d\n", tokens[token_index].line);
        break;
    }
}
