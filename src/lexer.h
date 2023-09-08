
#define DefineTokenList(X)\
X(EOF)\
X(Whitespace)\
X(Word)\
X(Keyword_Struct)\
X(Keyword_Enum)\
X(Keyword_With)\
X(Keyword_Namespace)\
X(Keyword_Alloc)\
X(Keyword_Sizeof)\
X(Keyword_Let)\
X(Keyword_Include)\
X(Keyword_If)\
X(Keyword_Else)\
X(Keyword_While)\
X(Keyword_For)\
X(Keyword_Switch)\
X(Keyword_Case)\
X(Keyword_Default)\
X(Keyword_Goto)\
X(Keyword_True)\
X(Keyword_False)\
X(Keyword_And)\
X(Keyword_Or)\
X(Keyword_Null)\
X(Keyword_Continue)\
X(Keyword_Break)\
X(Keyword_Return)\
X(Keyword_Type)\
X(Keyword_As)\
X(Keyword_Const)\
X(Integer)\
X(Decimal)\
X(String)\
X(Char)\
X(Comment)\
X(MultiComment)\
X(Comma)\
X(Period)\
X(Dotdot)\
X(Semicolon)\
X(Colon)\
X(QuestionMark)\
X(ExclamationMark)\
X(At)\
X(Tilde)\
X(Ampersand)\
X(Pipe)\
X(Caret)\
X(LeftShift)\
X(RightShift)\
X(OpenCurl)\
X(CloseCurl)\
X(OpenParen)\
X(CloseParen)\
X(OpenSquare)\
X(CloseSquare)\
X(LessThan)\
X(GreaterThan)\
X(LessThanOrEqual)\
X(GreaterThanOrEqual)\
X(Equals)\
X(NotEquals)\
X(Assign)\
X(Plus)\
X(Minus)\
X(Mul)\
X(Div)\
X(Mod)\
X(PlusPlus)\
X(MinusMinus)\
X(PlusAssign)\
X(MinusAssign)\
X(MulAssign)\
X(DivAssign)\
X(ModAssign)\
X(BitAndAssign)\
X(BitOrAssign)\
X(BitXorAssign)

#define TokenEnumEntry(token) Tok_##token,
typedef enum TokenType {
    DefineTokenList(TokenEnumEntry)
    TokenType_Count
} TokenType;
#undef TokenEnumEntry

#define TokenString(token) [Tok_##token] = #token,
static char* TokenType_Names[] = {
    DefineTokenList(TokenString)
};
#undef TokenEnumEntry

// signifies a string thats stored in the string-table
typedef u32 Identifier;

typedef union Tokendata {
    Identifier string;
    u64 integer;
    f64 decimal;
    char character;
} Tokendata;

typedef struct Token {
    TokenType type;
    u32 line;
    Tokendata data;
} Token;

