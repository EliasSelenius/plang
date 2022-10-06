
typedef enum TokenType {
    Tok_Whitespace,
    Tok_Word,

    Tok_Keyword_Struct,
    Tok_Keyword_With,
    Tok_Keyword_Namespace,
    Tok_Keyword_Alloc,
    Tok_Keyword_Sizeof,
    Tok_Keyword_Let,

    Tok_Keyword_If,
    Tok_Keyword_Else,
    Tok_Keyword_While,
    Tok_Keyword_For,
    Tok_Keyword_Switch,
    Tok_Keyword_Case,
    Tok_Keyword_Default,
    Tok_Keyword_Goto,

    Tok_Keyword_True,
    Tok_Keyword_False,
    Tok_Keyword_And,
    Tok_Keyword_Or,

    Tok_Keyword_Null,
    Tok_Keyword_Continue,
    Tok_Keyword_Break,
    Tok_Keyword_Return,
    Tok_Keyword_Declare,
    Tok_Keyword_Type,
    Tok_Keyword_As,
    Tok_Keyword_Const,

    Tok_Integer,
    Tok_Decimal,
    Tok_String,
    Tok_Char,

    Tok_Comment,
    Tok_MultiComment,

    Tok_Comma,
    Tok_Period,
    Tok_Semicolon,
    Tok_Colon,
    Tok_QuestionMark,
    Tok_ExclamationMark,
    Tok_At,
    Tok_Tilde,
    Tok_Ampersand,
    Tok_Pipe,
    Tok_Caret,

    Tok_LeftShift,
    Tok_RightShift,

    Tok_OpenCurl,
    Tok_CloseCurl,
    Tok_OpenParen,
    Tok_CloseParen,
    Tok_OpenSquare,
    Tok_CloseSquare,

    Tok_LessThan,
    Tok_GreaterThan,
    Tok_LessThanOrEqual,
    Tok_GreaterThanOrEqual,
    Tok_Equals,
    Tok_NotEquals,
    Tok_Assign,

    Tok_Plus,
    Tok_Minus,
    Tok_Mul,
    Tok_Div,
    Tok_Mod,

    Tok_PlusPlus,
    Tok_MinusMinus,

    Tok_PlusAssign,
    Tok_MinusAssign,
    Tok_MulAssign,
    Tok_DivAssign,
    Tok_ModAssign,
    Tok_BitAndAssign,
    Tok_BitOrAssign,
    Tok_BitXorAssign

} TokenType;


typedef struct Token {
    TokenType type;
    u32 line;

    union {
        u32 stringTableByteOffset;
        u64 integer;
        f64 decimal;
        char character;
    };
} Token;
