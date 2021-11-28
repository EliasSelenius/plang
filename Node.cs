using System;
using static Program;

using System.Collections.Generic;

abstract class Node {
    public int lineNum = -1;
    public Codeblock enclosingBlock;
    public PlangFile file;
    public abstract void transpile();

    public Node() {
        file = currentFile;
    }

}

class BlankNode : Node {
    public object value;
    public BlankNode(object t) => value = t; 
    public override void transpile() {
        //throw new Exception();
    }
}

class VariableDeclaration : Node {
    // if type is null it must be inferred
    public Typename type;
    public string name;
    public Expression value;


    public void inferType() {
        var exprType = value.getExpressedType();

        if (type is null) {
            type = exprType;
            return;
        }
        
        if (!type.typeEquals(exprType)) error(type + " and " + exprType + " are not compatible types");
    }

    public override void transpile() {
        if (type.isStruct(out StructDeclaration strDecl)) {
            cFile.addDependencie(strDecl.file);
        }
        cFile.writeline(type.transpile() + " " + name + " = " + value.transpileExpression() + ";");
    }
}

class Assignment : Node {
    public string name, assignmentOp;
    public Expression value;

    public override void transpile() {
        cFile.writeline(name + " " + assignmentOp + " " + value.transpileExpression() + ";");
    }
}


class Codeblock : Node {
    public List<Node> nodes;
    public Dictionary<string, VariableDeclaration> locals = new();

    /*public Typename getTypeOfLocal(string localName) {
        if (locals.ContainsKey(localName)) return locals[localName].type;
        return parentScope?.getCodeblock()?.getTypeOfLocal(localName);
    }*/

    public bool hasLocal(string localname, out Typename typename) {
        if (locals.ContainsKey(localname)) {
            typename = locals[localname].type;
            return true;
        }


        if (enclosingBlock != null) {
            bool ret = enclosingBlock.hasLocal(localname, out typename);
            return ret;
        }

        typename = null;
        return false;
    }

    public Codeblock(List<Node> nodes) {
        this.nodes = nodes;
        foreach (var item in nodes) item.enclosingBlock = this;
    }

    public void validate() {
        foreach (var item in nodes) {

            if (item is VariableDeclaration vd) {
                vd.inferType();
                locals.Add(vd.name, vd);
            } else if (item is Assignment ass) {
                if (ass.enclosingBlock.hasLocal(ass.name, out Typename t)) {
                    // TODO: type missmatch error
                } else {
                    error($"local \"{ass.name}\" does not exist.", ass);
                }
            } else if (item is Codeblock block) {
                block.validate();
            }
        }
    }

    /*public IEnumerable<ReturnStatement> getReturnStatements() {
        foreach (var n in nodes) if (n is ReturnStatement rs) yield return rs;

        foreach (var ss in subScopes) {
            foreach (var n in ss.getCodeblock().getReturnStatements()) yield return n;
        }
    }*/

    /*public void inferTypes() {
        foreach (var item in locals) item.Value.inferType();        
        foreach (var scope in subScopes) scope.getCodeblock().inferTypes();
    }*/

    public override void transpile() {
        foreach (var node in nodes) node.transpile();             
    }
}

class IfStatement : Codeblock {
    public BooleanExpression condition;

    public IfStatement(List<Node> nodes) : base(nodes) { }

    public override void transpile() {
        cFile.startblock($"if ({condition.transpileExpression()})");
        base.transpile();
        cFile.endblock();
    }
}

class PrintCommand : Node {
    public StringLiteral stringToPrint; 
    public override void transpile() {
        cFile.writeline($"printf(\"%s\", {stringToPrint.transpileExpression()});");
    }
}