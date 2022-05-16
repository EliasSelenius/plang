#include "parser.h"
#include "lexer.h"
#include "darray.h"

#include <stdlib.h> // malloc
#include <stdio.h>  // printf
#include <stdarg.h>

void expectBlock(Codeblock* scope);
Expression* parseExpression();
Expression* expectExpression();

static u32 token_index = 0;

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

static Token* anyof(u32 count, ...) {
    va_list args;
    va_start(args, count);    
    for (u32 i = 0; i < count; i++) {
        TokenType type = va_arg(args, TokenType);
        Token* tok = &tokens[token_index];
        if (tok->type == type) {
            token_index++;
            return tok;
        }
    }
    va_end(args);
    return null;
}


// tok_scan("t t t", Tok_Let, Tok_Word, Tok_Assign)
static bool tok_scan(char* format, ...) {
    u32 start = token_index;

    va_list args;
    va_start(args, format);

    TokenType type;
    char mode = ' ';

    u32 formatIndex = 0;
    while (true) {
        char cmd = format[formatIndex++];
        if (cmd == '\0') return true;
        if (cmd == ' ') continue;

        if (cmd == 't') {
            type = va_arg(args, TokenType);

            if (mode == ' ') {
                if (tokens[token_index].type == type) {
                    token_index++;
                    continue;
                } 
                
                goto fail;
            } else if (mode == '*') {
                while (tokens[token_index].type == type) token_index++;
                mode = ' ';
                continue;
            }

        }

        if (cmd == '*') {
            mode = '*'; continue;
        }

        printf("Wrong format in tok_scan()\n");
    }

    fail:
    token_index = start;
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

static void expectFuncCallArgs(FuncCall* func, Expression* funcExpr) {
    func->funcExpr = funcExpr;
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


static Expression* createLiteral(ExprType type) {
    LiteralExpression* lit = malloc(sizeof(LiteralExpression));
    lit->base.expressionType = type;
    lit->base.nodebase.lineNumber = tokens[token_index].line;
    lit->value = tokens[token_index].value;
    token_index++;
    return (Expression*)lit;
}

static Expression* parseLeafExpression() {
    Expression* res = null;

    switch (tokens[token_index].type) {
        case Tok_Word: {
            // valuepath or funcCall
            // TODO: line numbers 
            // ValuePath* value = parseValue();
            // if (tok(Tok_OpenParen)) {
            //     // func
            //     FuncCallExpression* funcex = malloc(sizeof(FuncCallExpression));
            //     funcex->base.expressionType = ExprType_FuncCall;

            //     expectFuncCallArgs(&funcex->call, value);
            //     res = (Expression*)funcex;
            // } else {
            //     // value
            //     ExpressionProxy* exp = malloc(sizeof(ExpressionProxy));
            //     exp->base.expressionType = ExprType_Variable;

            //     exp->node = value;
            //     res = (Expression*)exp;
            // }

            VariableExpression* ve = malloc(sizeof(VariableExpression));
            ve->base.expressionType = ExprType_Variable;
            ve->base.nodebase.lineNumber = tokens[token_index].line;
            ve->name = identifier();

            res = (Expression*)ve;

        } break;

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

            res = (Expression*)alloc;
        } break;

        case Tok_OpenParen: {
            token_index++;
            Expression* expr = expectExpression();
            expect(Tok_CloseParen);
            res = expr;
        } break;


        case Tok_Number:        res = createLiteral(ExprType_Literal_Number); break;
        case Tok_String:        res = createLiteral(ExprType_Literal_String); break;
        case Tok_Keyword_True:  res = createLiteral(ExprType_Literal_Bool); break;
        case Tok_Keyword_False: res = createLiteral(ExprType_Literal_Bool); break;
        case Tok_Keyword_Null:  res = createLiteral(ExprType_Literal_Null); break;

        default: return null;
    }

    // deref
    while (tok(Tok_Period)) {
        DerefOperator* deref = malloc(sizeof(DerefOperator));
        deref->base.expressionType = ExprType_Deref;
        deref->base.nodebase.lineNumber = tokens[token_index - 1].line;
        deref->expr = res;
        deref->name = identifier();
        res = (Expression*)deref;
    }

    // func call
    if (tok(Tok_OpenParen)) {
        FuncCall* call = malloc(sizeof(FuncCall));
        call->base.expressionType = ExprType_FuncCall;
        expectFuncCallArgs(call, res);
        res = (Expression*)call;
    }

    // TODO: dereferencing a function call?

    return res;
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

static bool isExpressionWriteable(Expression* expr) {
    
    switch (expr->expressionType) {
        case ExprType_Variable:
        case ExprType_Deref: return true;
        default: return false;
    }
}

/*
    a + b * c
    
      +
     / \
    a   *
       / \
      b   c

    a * b + c

          +
         / \
        *   c
       / \
      a   b



    a - b / c + d * e

            -
           / \
          a   +
             / \
         (/)     *
         / \    / \
        b   c  d   e

    at: ab
        -
       / \
      a   b

    at: abc
        -
       / \
      a  (\)
         / \
        b   c

    at: abcd
        -
       / \
      a   +
         / \
      (\)   d
      / \
     b   c

    at: abcde
            -
           / \
          a   +
             / \
         (/)     *
         / \    / \
        b   c  d   e


*/

static u32 operatorPriority(ExprType type) {
    switch (type) {

        case ExprType_BooleanAnd:
        case ExprType_BooleanOr:
            return 1;

        case ExprType_Less:
        case ExprType_Greater:
        case ExprType_LessEquals:
        case ExprType_GreaterEquals:
        case ExprType_Equals:
        case ExprType_NotEquals:
            return 2;

        case ExprType_Plus:
        case ExprType_Minus:
            return 3;
        case ExprType_Mul:
        case ExprType_Div:
            return 4;

        default: return 0; // not an operator
    }
}
inline u32 binaryOperatorPriority(BinaryExpression* expr) {
    return operatorPriority(expr->base.expressionType);
}

inline bool isBinaryExpression(Expression* expr) {
    return operatorPriority(expr->expressionType) != 0;
}

static Expression* expectLeafExpression() {
    Expression* res = parseLeafExpression();
    if (!res)
        error("..."); // TODO: fill out error message
    return res;
}

inline bool isOperator(TokenType type) {
    return (type >= Tok_Plus && type <= Tok_Div)
        || (type >= Tok_LessThan && type <= Tok_Equals)
        || (type == Tok_Keyword_And)
        || (type == Tok_Keyword_Or)
        || (type == ExprType_NotEquals);
}

static BinaryExpression* appendBinaryExpression(BinaryExpression* target, BinaryExpression* addition) {
    u32 targetPriority = binaryOperatorPriority(target);
    u32 additionPriority = binaryOperatorPriority(addition);

    if (additionPriority > targetPriority) {
        if (isBinaryExpression(target->right)) {
            target->right = (Expression*)appendBinaryExpression((BinaryExpression*)target->right, addition);
            return target;
        } else {
            addition->left = target->right;
            target->right = (Expression*)addition;
            return target;
        }
    } else {
        addition->left = (Expression*)target;
        return addition;
    }

    return null; // should never happen
}

static Expression* parseExpression() {
    Expression* a = parseLeafExpression();
    if (!a) return null;

    TokenType tokentype = tokens[token_index].type;
    if (!isOperator(tokentype)) return a;
    token_index++;

    BinaryExpression* root = malloc(sizeof(BinaryExpression));
    root->base.expressionType = (ExprType)tokentype; // safe to make cast since isOperator ensures only correct values for tokentype
    root->left = a;
    root->right = expectLeafExpression();

    tokentype = tokens[token_index].type;
    while (isOperator(tokentype)) {
        token_index++;

        BinaryExpression* op = malloc(sizeof(BinaryExpression));
        op->base.expressionType = (ExprType)tokentype; // safe to make cast since isOperator ensures only correct values for tokentype
        op->right = expectLeafExpression();

        root = appendBinaryExpression(root, op);

        tokentype = tokens[token_index].type;
    }

    return (Expression*)root;
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



        // case Tok_Keyword_Let: {
        //     res = (Statement*)expectVarDecl();
        //     semicolon();
        // } break;
        // case Tok_Word: {
        //     TokenType nextToken = tokens[token_index + 1].type;
        //     if (nextToken == Tok_Mul || nextToken == Tok_Word) {
        //         // var decl
        //         res = (Statement*)expectVarDecl();
        //     } else {
        //         // assignment or funcCall

        //         // ValuePath* valuePath = parseValue();


        //         switch (tokens[token_index].type) {
        //             case Tok_OpenParen: {
        //                 token_index++;
        //                 // funcCall
        //                 FuncCallStatement* funcCall = malloc(sizeof(FuncCallStatement));
        //                 funcCall->base.statementType = Statement_FuncCall;
        //                 expectFuncCallArgs(&funcCall->call, valuePath);
        //                 res = (Statement*)funcCall;
        //             } break;

        //             case Tok_PlusAssign:
        //             case Tok_MinusAssign:
        //             case Tok_MulAssign:
        //             case Tok_DivAssign:
        //             case Tok_Assign: {
        //                 // assignment

        //                 Assignement* ass = malloc(sizeof(Assignement));
        //                 ass->base.statementType = Statement_Assignment;
        //                 ass->assignee = valuePath;
        //                 ass->assignmentOper = tokens[token_index++].type;
        //                 ass->expr = expectExpression();
        //                 res = (Statement*)ass;
        //             } break;

        //             default: 
        //                 unexpectedToken(); 
        //                 return null;
        //         }
        //     }

        //     semicolon();
        // } break;
        
        case Tok_Keyword_Let: {
            res = (Statement*)expectVarDecl();
            semicolon();
        } break;

        default: {

            // id id | id any(*) id

            { // declaration
                u32 i = token_index;
                if (tokens[i++].type == Tok_Word) {
                    while (tokens[i].type == Tok_Mul) i++;
                    if (tokens[i++].type == Tok_Word) {
                        // confirmed declaration

                        VarDecl* decl = malloc(sizeof(VarDecl));
                        decl->base.statementType = Statement_Declaration;
                        decl->mustInferType = false;
                        decl->type = expectType();
                        decl->name = identifier();
                        decl->assignmentOrNull = tok(Tok_Assign) ? expectExpression() : null;

                        semicolon();
                        res = (Statement*)decl;
                        break;
                    }
                }
            }


            Expression* expr = parseExpression();
            if (expr) {
                switch (expr->expressionType) {
                    case ExprType_FuncCall: {                        
                        StatementExpression* staExpr = malloc(sizeof(StatementExpression));
                        staExpr->base.statementType = Statement_Expression;
                        staExpr->base.nodebase.lineNumber = expr->nodebase.lineNumber;
                        staExpr->expr = expr;
                        res = (Statement*)staExpr;
                    } break;
                    case ExprType_Variable:
                    case ExprType_Deref: {

                        Token* token = anyof(5, Tok_Assign, Tok_PlusAssign, Tok_MinusAssign, Tok_MulAssign, Tok_DivAssign);
                        if (token) {
                            Assignement* ass = malloc(sizeof(Assignement));
                            ass->base.statementType = Statement_Assignment;
                            ass->assigneeExpr = expr;
                            ass->assignmentOper = token->type;
                            ass->expr = expectExpression();
                            res = (Statement*)ass;
                        } else {
                            error("Expected an assignment.");
                        }

                    } break;
                    default:
                        error("This expression is all by its lonesome.");
                        return null;
                }
                semicolon();
                break;
            }

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
    if (!functions) {
        functions = darrayCreate(PlangFunction);
        functionDeclarations = darrayCreate(FuncDeclaration);
        structs = darrayCreate(PlangStruct);
        globalVariables = darrayCreate(VarDecl);
    }

    // token_index = 0;

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
