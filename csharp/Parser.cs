using System.Text.RegularExpressions;

//record Token(string value, int lineNum);

/*
    tokens
        - whitespace
        - number
        - string
        - true/false
        - words
        - {} () [] 
        - + - * /
        - , .
        - <> 


*/

struct Token {
    public string value;
    public int lineNum;

} 

static class NewParser {
    static Regex r;

    public static List<Token> lex(string[] lines) {
        var tokens = new List<Token>();

        for (int lineIndex = 0; lineIndex < lines.Length; lineIndex++) {
            int lineNum = lineIndex + 1;
            string line = lines[lineIndex];
                        


            tokens.Add(new Token { value = line, lineNum = lineNum });
        }

        return tokens;
    }
}