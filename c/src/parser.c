#include "parser.h"
#include "lexer.h"
#include "darray.h"

#include <stdlib.h> // malloc
#include <stdio.h>  // printf
#include <stdarg.h>

void expectBlock(Codeblock* scope);
Expression* parseExpression();
Expression* expectExpression();

static u32 token_index;

PlangFunction* functions = null;
FuncDeclaration* functionDeclarations = null;
PlangStruct* structs = null;
VarDecl* globalVariables = null;

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

static void errorLine(u32 lineNum, char* format, ...) {
    printf("Error Ln%d: ", lineNum);

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

    if (tok(Tok_OpenSquare)) {
        res->index = expectExpression();
        expect(Tok_CloseSquare);
    }

    if (tok(Tok_Period)) {
        res->next = parseValue();
    }

    return res;
}

static void expectFuncCallArgs(FuncCall* func, ValuePath* valuePath) {
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
}

inline bool isOperator(TokenType type) {
    return type >= Tok_Plus && type <= Tok_Div;
}

static Expression* createLiteral(ExprType type) {
    LiteralExpression* lit = malloc(sizeof(LiteralExpression));
    lit->base.expressionType = type;
    lit->base.nodebase.lineNumber = tokens[token_index].line;
    lit->value = tokens[token_index].value;
    token_index++;
    return (Expression*)lit;
}

static Expression* parseLeafExpression() {
    switch (tokens[token_index].type) {
        case Tok_Word: {
            // valuepath or funcCall
            // TODO: line numbers 
            ValuePath* value = parseValue();
            if (tok(Tok_OpenParen)) {
                // func
                FuncCallExpression* funcex = malloc(sizeof(FuncCallExpression));
                funcex->base.expressionType = ExprType_FuncCall;

                expectFuncCallArgs(&funcex->call, value);
                return (Expression*)funcex;
            } else {
                // value
                ExpressionProxy* exp = malloc(sizeof(ExpressionProxy));
                exp->base.expressionType = ExprType_Variable;

                exp->node = value;
                return (Expression*)exp;
            }
        }

        case Tok_Keyword_Alloc: {
            AllocExpression* alloc = malloc(sizeof(AllocExpression));
            alloc->base.expressionType = ExprType_Alloc;
            alloc->base.nodebase.lineNumber = tokens[token_index].line;

            token_index++;
            alloc->type = expectType();
            alloc->sizeExpr = null;
            if (tok(Tok_OpenSquare)) {
                alloc->sizeExpr = expectExpression();
                expect(Tok_CloseSquare);
            }

            return (Expression*)alloc;
        }

        case Tok_OpenParen: {
            token_index++;
            Expression* expr = expectExpression();
            expect(Tok_CloseParen);
            return expr;
        }


        case Tok_Number:        return createLiteral(ExprType_Literal_Number);
        case Tok_String:        return createLiteral(ExprType_Literal_String);
        case Tok_Keyword_True:  return createLiteral(ExprType_Literal_Bool);
        case Tok_Keyword_False: return createLiteral(ExprType_Literal_Bool);
        case Tok_Keyword_Null:  return createLiteral(ExprType_Literal_Null);

        default: return null;
    }
    return null;
}

static Expression* testForTernary(Expression* expr) {
    if (!tok(Tok_QuestionMark)) return expr;

    TernaryExpression* ter = malloc(sizeof(TernaryExpression));
    ter->base.expressionType = ExprType_Ternary;
    ter->base.nodebase.lineNumber = expr->nodebase.lineNumber;

    ter->condition = expr;
    ter->thenExpr = expectExpression();
    expect(Tok_Colon);
    ter->elseExpr = expectExpression();

    return (Expression*)ter;
}

// returns null if it fails to parse an expression
static Expression* parseExpression() {
    Expression* leaf = parseLeafExpression();
    if (!leaf) return null;

    u32 num = 0;
    Expression* temp[16]; // TODO: these arrays needs to go.
    temp[num++] = leaf;
    StrSpan ops[15];

    while (isOperator(tokens[token_index].type)) {
        ops[num-1] = tokens[token_index++].value;
        temp[num++] = parseLeafExpression(); // TODO: test for null
    }

    if (num == 1) return testForTernary(leaf);

    ArithmeticExpression* arith = malloc(sizeof(ArithmeticExpression));
    arith->base.expressionType = ExprType_Arithmetic;
    arith->base.nodebase.lineNumber = leaf->nodebase.lineNumber;
    arith->count = num;
    arith->operators = malloc(sizeof(StrSpan) * (num-1));
    arith->subExpressions = malloc(sizeof(Expression*) * num);

    for (u32 i = 0; i < num-1; i++) arith->operators[i] = ops[i];
    for (u32 i = 0; i < num; i++) arith->subExpressions[i] = temp[i];
    
    return testForTernary((Expression*)arith);
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


static IfStatement* expectIfStatement() {
    IfStatement* res = malloc(sizeof(IfStatement));
    res->base.statementType = Statement_If;
    res->next = null;

    expect(Tok_OpenParen);
    res->condition = expectExpression();
    expect(Tok_CloseParen);

    expectBlock(&res->scope);

    if (tok(Tok_Keyword_Else)) {
        if (tok(Tok_Keyword_If)) {
            // TODO: line number
            res->next = expectIfStatement();
        } else {
            IfStatement* elseStatement = malloc(sizeof(IfStatement));
            elseStatement->base.statementType = Statement_If;
            elseStatement->condition = null;
            elseStatement->next = null;
            expectBlock(&elseStatement->scope);

            res->next = elseStatement;
        }
    }

    return res;
}

static VarDecl* expectVarDecl() {
    VarDecl* decl = malloc(sizeof(VarDecl));
    decl->base.statementType = Statement_Declaration;

    if (tok(Tok_Keyword_Let)) {
        decl->mustInferType = true;
    } else {
        decl->mustInferType = false;
        decl->type = expectType();
    }

    decl->name = identifier();

    decl->assignmentOrNull = null;
    if (tok(Tok_Assign)) {
        decl->assignmentOrNull = expectExpression();
    } else if (decl->mustInferType) {
        error("Variable \"%.*s\" must be assigned to, to be type inferred.", decl->name.length, decl->name.start);
    }

    return decl;
}

static Statement* expectStatement() {
    Statement* res = null;

    u32 startingLineNum = tokens[token_index].line;

    switch (tokens[token_index].type) {
        case Tok_Keyword_Let: {
            res = (Statement*)expectVarDecl();
            semicolon();
        } break;
        case Tok_Word: {
            TokenType nextToken = tokens[token_index + 1].type;
            if (nextToken == Tok_Mul || nextToken == Tok_Word) {
                // var decl
                res = (Statement*)expectVarDecl();
            } else {
                // assignment or funcCall

                ValuePath* valuePath = parseValue();

                switch (tokens[token_index].type) {
                    case Tok_OpenParen: {
                        token_index++;
                        // funcCall
                        FuncCallStatement* funcCall = malloc(sizeof(FuncCallStatement));
                        funcCall->base.statementType = Statement_FuncCall;
                        expectFuncCallArgs(&funcCall->call, valuePath);
                        res = (Statement*)funcCall;
                    } break;

                    case Tok_PlusEquals:
                    case Tok_MinusEquals:
                    case Tok_MulEquals:
                    case Tok_DivEquals:
                    case Tok_Assign: {
                        // assignment

                        Assignement* ass = malloc(sizeof(Assignement));
                        ass->base.statementType = Statement_Assignment;
                        ass->assignee = valuePath;
                        ass->assignmentOper = tokens[token_index++].type;
                        ass->expr = expectExpression();
                        res = (Statement*)ass;
                    } break;

                    default: 
                        unexpectedToken(); 
                        return null;
                }
            }

            semicolon();
        } break;
        

        case Tok_OpenCurl: {
            Scope* scope = malloc(sizeof(Scope));
            scope->base.statementType = Statement_Scope;
            expectBlock(&scope->codeblock);

            res = (Statement*)scope;
        } break;
        case Tok_Keyword_While: {
            token_index++;

            WhileStatement* whileStatement = malloc(sizeof(WhileStatement));
            whileStatement->base.statementType = Statement_While;

            expect(Tok_OpenParen);
            whileStatement->condition = expectExpression();
            expect(Tok_CloseParen);

            expectBlock(&whileStatement->scope);

            res = (Statement*)whileStatement;
        } break;
        case Tok_Keyword_If: {
            token_index++;
            res = (Statement*)expectIfStatement();
        } break;


        case Tok_Keyword_Continue: {
            res = malloc(sizeof(Statement));
            res->statementType = Statement_Continue;
            token_index++;
            semicolon();
        } break;
        case Tok_Keyword_Break: {
            res = malloc(sizeof(Statement));
            res->statementType = Statement_Break;
            token_index++;
            semicolon();
        } break;
        case Tok_Keyword_Return: {
            res = malloc(sizeof(ReturnStatement));
            res->statementType = Statement_Return;
            token_index++;
            ((ReturnStatement*)res)->returnExpr = parseExpression();
            semicolon();
        } break;

        default: {
            unexpectedToken();
            return null;
        }
    }

    // res should never be null up to this point

    res->nodebase.lineNumber = startingLineNum;

    return res;
}

static void expectBlock(Codeblock* scope) {

    expect(Tok_OpenCurl);

    scope->statements = darrayCreate(Statement*);

    while (!tok(Tok_CloseCurl)) {
        Statement* statement = expectStatement();
        if (statement) {
            darrayAdd(scope->statements, statement);
        } else {
            // there must have been an error.
        }
    }
}

static PlangStruct expectStruct() {
    PlangStruct stru;
    stru.name = identifier();
    stru.fields = darrayCreate(Field);

    expect(Tok_OpenCurl);

    do {
        Field field;
        field.nodebase.lineNumber = tokens[token_index].line;
        field.type = expectType();
        field.name = identifier();
        semicolon();

        darrayAdd(stru.fields, field);

    } while (tokens[token_index].type != Tok_CloseCurl);
    
    token_index++;

    return stru;
}

static void expectFuncArgList(FuncArg** arguments) {
    FuncArg arg;
    if (parseType(&arg.type)) {
        arg.name = identifier();
        
        *arguments = darrayCreate(FuncArg);
        darrayAdd(*arguments, arg);
        
        while (tok(Tok_Comma)) {
            arg.type = expectType();
            arg.name = identifier();
            darrayAdd(*arguments, arg);
        }
    }

    expect(Tok_CloseParen);
}

static void funcOrGlobal(bool typeinfer) {
    PlangType type;
    if (typeinfer) token_index++;
    else type = expectType();

    StrSpan name = identifier();

    if (tok(Tok_OpenParen)) {
        // function

        PlangFunction func;
        func.mustInferReturnType = typeinfer;
        func.decl.returnType = type;
        func.decl.name = name;
        func.decl.arguments = null;

        expectFuncArgList(&func.decl.arguments);
        
        expectBlock(&func.scope);
        darrayAdd(functions, func);

    } else {
        // global variable
        VarDecl decl;
        decl.assignmentOrNull = null;
        decl.name = name;
        decl.mustInferType = typeinfer;        
        decl.type = type;
        
        if (tok(Tok_Assign)) {
            decl.assignmentOrNull = expectExpression();
        } else if (typeinfer) {
            error("Global variable \"%.*s\" must be assigned to, to be type inferred.", decl.name.length, decl.name.start);
        }

        darrayAdd(globalVariables, decl);

        semicolon();
    }
}

u32 parse() {

    // TODO: PlangFile
    functions = darrayCreate(PlangFunction);
    functionDeclarations = darrayCreate(FuncDeclaration);
    structs = darrayCreate(PlangStruct);
    globalVariables = darrayCreate(VarDecl);

    token_index = 0;
    while (token_index < tokens_length) {

        switch (tokens[token_index].type) {
            case Tok_Keyword_Struct: {
                // struct
                u32 lineNum = tokens[token_index].line;
                token_index++;
                PlangStruct stru = expectStruct();
                stru.nodebase.lineNumber = lineNum;
                u32 struLen = darrayLength(structs);
                for (u32 i = 0; i < struLen; i++) {
                    if (spanEqualsSpan(structs[i].name, stru.name)) {
                        errorLine(stru.nodebase.lineNumber, "Struct \"%.*s\" is already defined.", stru.name.length, stru.name.start);
                        break;
                    }
                }
                
                darrayAdd(structs, stru);

            } break;
            
            case Tok_Keyword_Let: funcOrGlobal(true); break;
            case Tok_Word: funcOrGlobal(false); break;

            case Tok_Keyword_Declare: {
                // function declaration
                token_index++;
                FuncDeclaration funcDecl;
                funcDecl.returnType = expectType();
                funcDecl.name = identifier();
                expect(Tok_OpenParen);
                expectFuncArgList(&funcDecl.arguments);
                semicolon();

                darrayAdd(functionDeclarations, funcDecl);
            } break;
            
            default: {
                unexpectedToken();
            } break;
        }
    }

    return numberOfErrors;
}
