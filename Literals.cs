using pcombinator;
using static pcombinator.Parser;

class NumberLiteral : Expression {

    public Typename getExpressedType() {

        /*
        var t1 = 10;
        var t2 = 10f;
        var t3 = 10d;
        var t4 = 10m;
        */

        var type = typeSuffix switch {
            "f" => "float",
            "d" => "double",
            "u" => "uint",
            "l" => "long",
            "ul" => "ulong",
            _ => decimalPart switch {
                "" => "int",
                _ => "double"
            }
        };

        return new Typename(type, 0);
    }


    public readonly string sign, integerPart, decimalPart;
    public bool hasDecimalPart => !string.IsNullOrEmpty(decimalPart);
    public string typeSuffix;

    public NumberLiteral(string s, string i, string f, string end) {
        sign = s;
        integerPart = i;
        decimalPart = f;
        typeSuffix = end;
    }

    public string transpileExpression() {
        return sign + integerPart + (hasDecimalPart ? "." + decimalPart : "");
    }
}

class BooleanLiteral : BooleanExpression {
    public string value;
    public override string transpileExpression() {
        return value.Equals("true") ? "1" : "0";
    }
}

class StringLiteral : Expression {
    public Typename getExpressedType() => new Typename("char", 1);
    public string value;
    public string transpileExpression() {
        return value;
    }
}

class NullLiteral : Expression {
    public Typename getExpressedType() => Types.voidPtr;

    public string transpileExpression() => "((void*)0)";
}

static class Literals {
    
    public static Parser number;
    public static Parser doubleQuoteString;
    public static Parser boolean;
    public static Parser nullLiteral;

    public static Parser anyLiteralValue;

    static Literals() {

        Parser numLitSuffix = str("f") | "d" | "ul" | "u" | "l";
        number = (optional(minus) & digits & optional(period & digits) & optional(numLitSuffix)).map(res => {
            return new NumberLiteral(res[0] ?? "", res[1], res[2]?[1] ?? "", res[3] ?? "");
        });
        

        doubleQuoteString = regex("^(\".*?[^\\\\]\")").map(res => new StringLiteral { value = res });

        boolean = (str("true") | "false").map(res => new BooleanLiteral { value = res });

        nullLiteral = str("null").map((res, i) => new NullLiteral());

        anyLiteralValue = number | boolean | doubleQuoteString | nullLiteral;
    }
}