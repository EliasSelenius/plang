using static Program;
using pcombinator;
using static pcombinator.Parser;


interface Expression {
    Typename getExpressedType();
    string transpileExpression();
}

abstract class BooleanExpression : Expression {
    public Typename getExpressedType() => new Typename("bool", 0);
    public abstract string transpileExpression();
}

class BooleanBinaryExpression : BooleanExpression {
    public string operation;
    public BooleanExpression left, right;

    public override string transpileExpression() {
        throw new System.NotImplementedException();
    }
}

class ArithmeticExpression : Expression {
    public Typename getExpressedType() {
        var leftType = left.getExpressedType();
        var rightType = right.getExpressedType();
        if (leftType.typeEquals(rightType)) return leftType;

        throw new System.Exception($"Cannot do {leftType.transpile()} {operation} {rightType.transpile()}");
    }

    // + - * /
    public string operation;
    public Expression left, right;

    public string transpileExpression() {
        return "(" + left.transpileExpression() + " " + operation + " " + right.transpileExpression() + ")";
    }
}

class VarIdentifier : Expression {
    public Typename getExpressedType() {
        throw new();
    }

    public string name;

    public string transpileExpression() {
        return name;
    }
}

class DerefOperator : Expression {
    public Expression expr;
    public string memberName;

    //      .   ->
    public string operation;

    public Typename getExpressedType() {
        var exprType = expr.getExpressedType();
        
        if (Global.getStruct(exprType.structName, out StructDeclaration strd)) {
            var mem = strd.getMember(memberName);

            if (mem is null) error("'" + memberName + "' does not exist on struct '" + strd.name + "'");

            return mem.type;

        } 
        
        error("struct '" + exprType.structName + "' does not exist");
        return null;
    }

    public string transpileExpression() {
        return expr.transpileExpression() + operation + memberName;
    }
}

class AllocExpression : Expression {
    public Typename allocType;
    public string count;

    public Typename getExpressedType() => allocType with {
        numPointers = allocType.numPointers + 1
    };

    public string transpileExpression() {
        return "malloc(sizeof(" + allocType.transpile() + (string.IsNullOrEmpty(count) ? "))" : $") * {count})");
    }
}