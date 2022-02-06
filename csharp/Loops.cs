using pcombinator;
using static pcombinator.Parser;
using static Program;
using System.Collections.Generic;

abstract class Loooop : Codeblock {
    public Loooop(List<Node> nodes) : base(nodes) { }
}

class WhileLoop : Loooop {
    public BooleanExpression condition;

    public WhileLoop(List<Node> nodes) : base(nodes) { }

    public override void transpile() {
        cFile.startblock($"while ({condition.transpileExpression()})");
        base.transpile();
        cFile.endblock();
    }
}

abstract class LoopControllStatement : Node { }

class BreakStatemenet : LoopControllStatement {
    public override void transpile() => cFile.writeline("break;");
}

class ContinueStatement : LoopControllStatement {
    public override void transpile() => cFile.writeline("continue;");
}