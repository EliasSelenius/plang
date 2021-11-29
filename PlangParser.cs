using pcombinator;
using static pcombinator.Parser;

using System.Collections.Generic;


static class Plang {
 
    public static Parser mainParser;
    public static Parser identifier, funcPtrType, codeBlock = new(), argDevisor;
    public static Parser typeParser = new();

    // whitespace parsers
    public static Parser ws, optWs;


    public static Parser funcParser = new(), funcCall = new();


    public static Parser expressionParser = new();
    public static Parser arithmeticExpr = new();

    public static Parser deref;

    public static Parser boolExpression = new();


    public static Parser structParser = new();

    public static Parser moduleParser = new();


    static void setupFunctions() {

        Parser arg = (typeParser & whitespace & identifier).map(res => new FunctionArgument { type = res[0], name = res[2] });
        Parser arglist = inParentheses(arg / argDevisor).map(res => {
            var r = new FunctionArgument[res.Length];
            for (int i = 0; i < r.Length; i++) r[i] = res[i];
            return r;
        });


        funcParser.init((typeParser & whitespace & identifier & arglist & whitespace & codeBlock).map(res => new Function(
            name:res[2],
            retType:res[0],
            codeblock:new Codeblock(res[5]),
            arglist:res[3])));

        Parser funcCallArgs = inParentheses(expressionParser / argDevisor).map(res => {
            var r = new Expression[res.Length];
            for (int i = 0; i < r.Length; i++) r[i] = res[i];
            return r;
        });
        funcCall.init((identifier & funcCallArgs).map(res => new FunctionCall {
            funcName = res[0],
            arguments = res[1]
        }));
    }


    static void setupExpressions() {

        boolExpression.init(Literals.boolean);

        Parser allocExpr = ("alloc" & whitespace & typeParser & optional(inSquareBrackets(digits))).map(res => new AllocExpression {
            allocType = res[2],
            count = res[3]
        });

        Parser varid = identifier.map(res => new VarIdentifier { name = res });

        Parser derefop = optWs & (period | "->") & optWs;
        deref = (varid & derefop & identifier).map(res => new DerefOperator {
            expr = res[0], operation = res[2], memberName = res[4]
        });

        Parser leafExpr = Literals.anyLiteralValue | allocExpr | deref | varid;

        Parser arithmeticOp = plus | minus | "*" | "/";

        Parser genAritExpr(Parser parser) => (parser & ws & arithmeticOp & ws & parser).map(res => new ArithmeticExpression {
            operation = res[2],
            left = res[0],
            right = res[4]
        });
        arithmeticExpr.init(genAritExpr(leafExpr | inParentheses(arithmeticExpr)));

        expressionParser.init(arithmeticExpr | leafExpr);
    }

    public static void newExpressionsTest() {

        // writeable var
        Parser derefop = optWs & (period | "->") & optWs;
        Parser variable = identifier / derefop;

        // arithmetic expression
        Parser arithmeticOp = (optWs & (plus | minus | "*" | "/") & optWs).map(res => res[1]);
        Parser expr = new();

        // (Literals.number | expr) / arithmeticOp
        Parser arithmeticValue = Literals.number | expr;
        Parser arithmeticExpr = (arithmeticValue & many1(arithmeticOp & arithmeticValue)).map(res => {
            return res;
        });

        expr.init( inParentheses(expr) | arithmeticExpr);


        var r1 = expr.run("1 + 2 / 3");
        var r2 = expr.run("3 * (1 - 5 + 2) * 6 / 4");
        var r3 = expr.run("1");

    }


    static void setupStructDeclarations() {
        Parser member = (typeParser & whitespace & identifier & semicolon).map(res => new StructMember {
            name = res[2],
            type = res[0]
        });
        Parser structBody = inCurlyBrackets(+(member | ignore(whitespace))).map(res => {
            var r = new StructMember[res.Length];
            for (int i = 0; i < r.Length; i++) r[i] = res[i];
            return r;
        });



        structParser.init(("struct" & whitespace & identifier & whitespace & structBody).map(
            res => new StructDeclaration(name:res[2], members:res[4])));
    }


    static void runParserTests() {
        /*var fp1 = typeParser.run("(int => char*)");
        var fp2 = typeParser.run("(=> Gameobject*)");
        var fp3 = typeParser.run("(char*, int => Gameobject*)");
        var fp4 = typeParser.run("(float, int =>)");
        var fp5 = typeParser.run("(=>)");


        var f1 = typeParser.run("char*");
        var f2 = typeParser.run("(=>)");
        var f3 = typeParser.run("((=>) => int)");*/

        //var r1 = Expressions.deref.run("foo->child_count");
        //var r2 = Expressions.deref.run("foo.bar");


        var s = structParser.run("struct Test {  int x; Foo bar; }");

        var t1 = expressionParser.run("\"Hello\\\"World\"");
        var t2 = expressionParser.run("isVisible");
        var t3 = expressionParser.run("3.14");
        var t4 = expressionParser.run("true");
        
        var t5 = expressionParser.run("12 + 4.4");
        var t6 = expressionParser.run("(foo + 3) / 4.4");
        var t7 = expressionParser.run("((10 * foo) + 3) / (1 - 1)");

    }


    static Plang() {
        ws = whitespace;
        optWs = optional(ws);

        identifier = regex("^[_a-zA-Z][_a-zA-Z0-9]*");

        argDevisor = (optional(whitespace) & comma & optional(whitespace));

        Parser objectType = (identifier & many("*")).map(res => new Typename(res[0], res[1].Length)); 
        funcPtrType = inParentheses((typeParser / argDevisor) & optWs & "=>" & optWs & optional(typeParser));
        typeParser.init(funcPtrType | objectType);

        Parser assignment = (identifier & ws & str("=") & ws & expressionParser).map(res => {
            return new Assignment {
                name = res[0],
                assignmentOp = res[2],
                value = res[4]
            };
        });

        Parser vardeclaration = (("let" | typeParser) & ws & identifier & ws & "=" & ws & expressionParser).map(res => {
            return new VariableDeclaration {
                type = res[0] as Typename,
                name = res[2],
                value = res[6]
            };
        });

        Parser condition = (optWs & inParentheses(boolExpression) & optWs).map(res => res[1]);

        Parser postIfStatement = new();
        Parser elseStatement = ("else" & optWs & codeBlock).map(res => new ElseStatement(nodes:res[2]));
        Parser elseIfStatement = ("else if" & condition & codeBlock & optWs & postIfStatement).map(res => new ElifStatement(nodes:res[2]) {
            condition = res[1],
            postIf = res[4]
        });

        postIfStatement.init(optional(elseIfStatement | elseStatement));

        Parser ifStatement = ("if" & condition & codeBlock & optWs & postIfStatement).map(res => {
            return new IfStatement(nodes:res[2]) {
                condition = res[1],
                postIf = res[4]
            };
        });

        Parser whileLoopParser = ("while" & condition & codeBlock).map(res => new WhileLoop(nodes:res[2]) {
            condition = res[1]
        });


        Parser blockStatement = choice(
            ifStatement,
            whileLoopParser
        );


        Parser printCommand = ("print" & ws & Literals.doubleQuoteString).map(res => new PrintCommand {
            stringToPrint = res[2]
        });

        Parser returnStatement = ("return" & ws & expressionParser).map(res => new ReturnStatement {
            returnValue = res[2]
        });

        Parser lineComment = regex("^(//.*)");
        Parser multiLineComment = regex("^(/\\*(.|\\n)*?\\*/)");

        Parser statementEnd = semicolon.mapError((r, i) => {
            Program.error("Missing semicolon.", i);
            return null;
        });

        Parser statement = (choice(
            vardeclaration,
            assignment,
            funcCall,
            printCommand,
            returnStatement

        ) & statementEnd).map(res => res[0]);

        codeBlock.init(inCurlyBrackets(+(ignore(ws) | statement | blockStatement | lineComment | multiLineComment)).map(res => {
            List<Node> nodes = new();

            foreach (var item in res) {
                if (item is Node) nodes.Add(item);

                // temporary way to include items that are not of the Node type
                else nodes.Add(new BlankNode(item));
            }

            return nodes;
        }));


        setupFunctions();
        setupExpressions();
        setupStructDeclarations();


        { // modules
            Parser moduleBody = inCurlyBrackets(many(ignore(ws) | funcParser | structParser)).map(res => {
                var r = new Node[res.Length];
                for (int i = 0; i < r.Length; i++) r[i] = res[i];
                return r;
            });

            moduleParser.init(("module" & ws & identifier & optWs & moduleBody).map(res => new Module {
                name = res[2],
                body = res[4]
            }));
        }

        mainParser = all(ignore(ws) | funcParser | structParser | moduleParser);

    }
}