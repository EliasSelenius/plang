using pcombinator;
using static pcombinator.Parser;
using static Program;
using System.Collections.Generic;


class WhileLoop : Codeblock {
    public BooleanExpression condition;

    public WhileLoop(List<Node> nodes) : base(nodes) { }

    public override void transpile() {
        cFile.startblock($"while ({condition.transpileExpression()})");
        base.transpile();
        cFile.endblock();
    }
}