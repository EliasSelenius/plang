#include "parser.h"
#include "lexer.h"
#include "darray.h"

// included for NULL
#include <stdlib.h>
#include <stdio.h>

#include <stdarg.h>

void expectBlock(Codeblock* scope);
Expression* parseExpression();
Expression* expectExpression();

static u32 token_index;

PlangFunction* functions = null;
PlangStruct* structs = null;

static u32 numberOfErrors;
static void error(char* format, ...) {
    printf("Error Ln%d: ", tokens[token_index].line);

    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    printf("\n");
    numberOfErrors++;
}


// asserts the existence of a semicolon
inline void semicolon() {
    if (tokens[token_index].type != Tok_Semicolon) {     
        error("Expected semicolon, but got \"%.*s\" instead.",
            tokens[token_index].value.length,
            tokens[token_index].value.start);
        return;
    }
    token_index++;
}

static StrSpan identifier() {
    Token* token = &tokens[token_index];
    if (token->type != Tok_Word) {
        error("\"%.*s\" is not a valid identifier.",
            token->value.length,
            token->value.start); 
    }
    token_index++;
    return token->value;
}

static void expect(TokenType type) {
    if (tokens[token_index].type != type) {
        // TODO: get TokenType as string
        error("Expected token type %d, but got \"%.*s\" instead.",
            type,
            tokens[token_index].value.length,
            tokens[token_index].value.start);

        return;
    }
    token_index++;
}

static void unexpectedToken() {

    error("Unexpected token \"%.*s\".",
        tokens[token_index].value.length,
        tokens[token_index].value.start);

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
        error("Expected type, but got \"%.*s\" instead.",
            tokens[token_index].value.length,
            tokens[token_index].value.start);
    }
    return res;
}


#define assertToken(tok) if (tokens[token_index++].type != tok) goto failCase;

#define nextToken(tok) (tokens[token_index++].type == tok)

inline bool tok(TokenType type) {
    if (tokens[token_index].type == type) {
        token_index++;
        return true;
    }
    return false;
}


static ValuePath* parseValue() {
    if (tokens[token_index].type != Tok_Word) return null;

    ValuePath* res = malloc(sizeof(ValuePath));
    res->name = tokens[token_index].value;
    res->type = null;
    res->next = null;
    res->index = null;
    token_index++;

    if (tokens[token_index].type == Tok_OpenSquare) {
        token_index++;
        res->index = expectExpression();
        expect(Tok_CloseSquare);
    }

    if (tokens[token_index].type == Tok_Period) {
        token_index++;
        res->next = parseValue();
    }

    return res;
}

static FuncCall* expectFuncCallArgs(ValuePath* valuePath) {
    FuncCall* func = malloc(sizeof(FuncCall));
    func->valuePath = valuePath;
    func->function = null;

    Expression* expr = parseExpression();
    if (expr) {
        func->args = darrayCreate(Expression*);
        darrayAdd(func->args, expr);

        while (tok(Tok_Comma)) {
            expr = expectExpression();
            darrayAdd(func->args, expr);
        }
    } else {
        func->args = null;
    }

    expect(Tok_CloseParen);

    return func;
}

inline bool isOperator(TokenType type) {
    return type >= Tok_Plus && type <= Tok_Div;
}


static Expression* parseLeafExpression() {
    Token* token = &tokens[token_index++];
    ExprType eType;

    switch (token->type) {
        case Tok_Word: {
            token_index--;
            Expression* expr = malloc(sizeof(Expression));
            ValuePath* valuePath = parseValue();

            if (tok(Tok_OpenParen)) {
                expr->expressionType = ExprType_FuncCall;
                expr->node = expectFuncCallArgs(valuePath);
            } else {
                expr->expressionType = ExprType_Variable;
                expr->node = valuePath;
            }

            return expr;
        } break;
        case Tok_Number: {
            eType = ExprType_Number_Literal;
        } break;
        case Tok_String: {
            eType = ExprType_String_Literal;
        } break;

        case Tok_Keyword_False:
        case Tok_Keyword_True: {
            eType = ExprType_Bool_Literal;
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

static Expression* testForTernary(Expression* expr) {
    if (tokens[token_index].type != Tok_QuestionMark) return expr;
    token_index++;

    TernaryExpression* ter = malloc(sizeof(TernaryExpression));
    ter->condition = expr;
    ter->thenExpr = expectExpression();
    expect(Tok_Colon);
    ter->elseExpr = expectExpression();

    Expression* res = malloc(sizeof(Expression));
    res->node = ter;
    res->expressionType = ExprType_Ternary;
    return res;
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

    if (i == 1) return testForTernary(leafExpr);


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


    return testForTernary(expr);
}

static Expression* expectExpression() {
    Expression* res = parseExpression();
    if (res == null) {
        error("Expected expression, but got \"%.*s\" instead.",
            tokens[token_index].value.length,
            tokens[token_index].value.start);
        token_index++;
    }
    return res;
}


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


static IfStatement* expectIfStatement() {
    IfStatement* res = malloc(sizeof(IfStatement));
    res->next = NULL;

    expect(Tok_OpenParen);
    res->condition = expectExpression();
    expect(Tok_CloseParen);

    expectBlock(&res->scope);

    if (tokens[token_index].type == Tok_Keyword_Else) {
        token_index++;
        if (tokens[token_index].type == Tok_Keyword_If) {
            token_index++;
            res->next = expectIfStatement();
        } else {
            res->next = malloc(sizeof(IfStatement));
            res->next->condition = NULL;
            expectBlock(&res->next->scope);
        }
    }

    return res;
}


static bool parseStatement(Statement* statement) {

    if ( (statement->node = parseVarDecl()) ) {
        statement->statementType = Statement_Declaration;
        semicolon();
    }
    else if (tokens[token_index].type == Tok_Word) {

        ValuePath* valuePath = parseValue();
        switch (tokens[token_index].type) {
            case Tok_Assign:
            case Tok_PlusEquals:
            case Tok_MinusEquals:
            case Tok_MulEquals:
            case Tok_DivEquals: {
                Assignement ass;
                ass.assignee = valuePath;
                ass.assignmentOper = tokens[token_index].type;

                token_index++;
                ass.expr = expectExpression();
                semicolon();

                statement->statementType = Statement_Assignment;
                Assignement* ap = statement->node = malloc(sizeof(Assignement));
                *ap = ass;
            } break;

            case Tok_OpenParen: {
                token_index++;
                statement->statementType = Statement_FuncCall;
                statement->node = expectFuncCallArgs(valuePath);                
                semicolon();
            } break;
            
            default:
                token_index--; // why the fuck?
                return false;
        }
    }
    else if (tokens[token_index].type == Tok_Keyword_While) {
        token_index++;
        statement->statementType = Statement_While;

        WhileStatement* res = malloc(sizeof(WhileStatement));

        expect(Tok_OpenParen);
        res->condition = expectExpression();
        expect(Tok_CloseParen);

        expectBlock(&res->scope);

        statement->node = res;
    }
    else if (tokens[token_index].type == Tok_Keyword_If) {
        token_index++;
        statement->statementType = Statement_If;
        statement->node = expectIfStatement();
    }    
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

    expect(Tok_OpenCurl);

    scope->statements = darrayCreate(Statement);

    Statement statement;
    while (tokens[token_index].type != Tok_CloseCurl) {
        if (!parseStatement(&statement)) {
            unexpectedToken();
        }

        darrayAdd(scope->statements, statement);
    }

    token_index++;
}

static void expectStruct() {
    PlangStruct stru;
    stru.name = identifier();
    stru.fields = darrayCreate(Field);

    expect(Tok_OpenCurl);

    do {
        Field field;
        field.type = expectType();
        field.name = identifier();
        semicolon();

        darrayAdd(stru.fields, field);

    } while (tokens[token_index].type != Tok_CloseCurl);
    
    token_index++;

    darrayAdd(structs, stru);
}


u32 parse() {

    // TODO: PlangFile
    functions = darrayCreate(PlangFunction);
    structs = darrayCreate(PlangStruct);

    token_index = 0;
    while (token_index < tokens_length) {

        switch (tokens[token_index].type) {
            case Tok_Keyword_Struct: {
                // struct
                token_index++;
                expectStruct();
            } break;
            
            
            case Tok_Word: {
                // function / global variable

                PlangType type = expectType();
                StrSpan name = identifier();

                if (tok(Tok_OpenParen)) {
                    // function

                    PlangFunction func;
                    func.returnType = type;
                    func.name = name;
                    func.arguments = null;

                    FuncArg arg;
                    if (parseType(&arg.type)) {
                        arg.name = identifier();
                        
                        func.arguments = darrayCreate(FuncArg);
                        darrayAdd(func.arguments, arg);
                        
                        while (tok(Tok_Comma)) {
                            arg.type = expectType();
                            arg.name = identifier();
                            darrayAdd(func.arguments, arg);
                        }
                    }

                    expect(Tok_CloseParen);
                    
                    expectBlock(&func.scope);
                    darrayAdd(functions, func);

                } else {
                    // global variable
                    semicolon();
                    printf("global variables are not implemented yet.\n");
                }
            } break;
            
            default: {
                // error("Did not expect token \"%.*s\" here.",
                //     tokens[token_index].value.length,
                //     tokens[token_index].value.start);
                // token_index++;
                unexpectedToken();
            } break;
        }
    }

    return numberOfErrors;
}
