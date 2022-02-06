using pcombinator;
using static pcombinator.Parser;
using System.Collections.Generic;
using static Program;


class FunctionArgument {
    public Typename type;
    public string name;
    public string transpile() {
        return $"{type.transpile()} {name}";
    }
}

class Function : Node {


    public Typename returnType;
    public string name;
    public Codeblock codeblock;
    public FunctionArgument[] arglist;

    public Function(
        string name,
        Typename retType,
        Codeblock codeblock,
        FunctionArgument[] arglist
    ) {
        this.name = name;
        this.returnType = retType;
        this.codeblock = codeblock;
        this.arglist = arglist;

        Global.addFunction(this);        
    }
    

    string constructSignature() {
        string args() {
            if (arglist.Length == 0) return "()";
            var res = "(" + arglist[0].transpile();
            for (int i = 1; i < arglist.Length; i++) res += ", " + arglist[i].transpile();
            return res + ")";
        }

        return returnType.transpile() + " " + name + args();
    }

    public override void transpile() {
        var signature = constructSignature();

        hFile.writeline(signature + ";");

        cFile.startblock(signature);
        codeblock.transpile();
        cFile.endblock();
    }
}

class ReturnStatement : Node {
    public Expression returnValue;
    public override void transpile() {
        cFile.writeline("return " + returnValue.transpileExpression() + ";");
    }
}

class FunctionCall : Node, Expression {
    public string funcName;
    public Expression[] arguments;

    public Function getFunction() => throw new System.NotImplementedException();

    public Typename getExpressedType() {
        return getFunction().returnType;
    }

    public string transpileExpression() {
        //cFile.include(Global.fileDependencies[funcName].hFilepath);
        cFile.addDependencie(Global.fileDependencies[funcName]);

        string args() {
            if (arguments.Length == 0) return "()";
            var res = "(" + arguments[0].transpileExpression();
            for (int i = 1; i < arguments.Length; i++) res += ", " + arguments[i].transpileExpression();
            return res + ")";
        }

        return funcName + args();
    }

    public override void transpile() {
        cFile.writeline(transpileExpression() + ";");
    }
}